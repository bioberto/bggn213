Class17
================
Alberto Carreno
3/4/2020

Here we analyze infection data for the 2019 novel Coronavirus COVID-19
(2019-nCoV) epidemic. The raw data is pulled from the Johns Hopkins
University Center for Systems Science and Engineering (JHU CCSE)
Coronavirus repository.

A CSV file is available here
<https://github.com/RamiKrispin/coronavirus-csv>

``` r
url <- "https://tinyurl.com/COVID-2019"
virus <- read.csv(url)

tail(virus)
```

    ##      Province.State Country.Region      Lat     Long       date cases      type
    ## 2881         Shanxi Mainland China  37.5777 112.2922 2020-03-05     2 recovered
    ## 2882        Sichuan Mainland China  30.6171 102.7103 2020-03-05    19 recovered
    ## 2883        Tianjin Mainland China  39.3054 117.3230 2020-03-05     4 recovered
    ## 2884       Victoria      Australia -37.8136 144.9631 2020-03-05     3 recovered
    ## 2885       Xinjiang Mainland China  41.1129  85.2401 2020-03-05     1 recovered
    ## 2886       Zhejiang Mainland China  29.1832 120.0934 2020-03-05    10 recovered

> Q1. How many total infected cases are there around the world?

``` r
total_cases <- sum(virus$cases)
total_cases
```

    ## [1] 155031

``` r
table(virus$type)
```

    ## 
    ## confirmed     death recovered 
    ##      1593       212      1081

``` r
sum(virus$type == "death")
```

    ## [1] 212

``` r
inds <-  virus$type == 'death'
death_cases <- sum(virus[inds,"cases"])
death_cases
```

    ## [1] 3348

> Q2. how many deaths linked to infected cases have there been?

``` r
inds <-  virus$type == 'death'
death_cases <- sum(virus[inds,"cases"])
death_cases
```

    ## [1] 3348

``` r
round(death_cases/total_cases*100,2)
```

    ## [1] 2.16

> Q3. What is the overall death rate?

``` r
round(death_cases/total_cases*100,2)
```

    ## [1] 2.16

``` r
virus[,]
```

    ##                                   Province.State         Country.Region
    ## 1                                                                 Japan
    ## 2                                                           South Korea
    ## 3                                                              Thailand
    ## 4                                          Anhui         Mainland China
    ## 5                                        Beijing         Mainland China
    ## 6                                      Chongqing         Mainland China
    ## 7                                         Fujian         Mainland China
    ## 8                                      Guangdong         Mainland China
    ## 9                                        Guangxi         Mainland China
    ## 10                                       Guizhou         Mainland China
    ## 11                                        Hainan         Mainland China
    ## 12                                         Hebei         Mainland China
    ## 13                                         Henan         Mainland China
    ## 14                                         Hubei         Mainland China
    ## 15                                         Hunan         Mainland China
    ## 16                                       Jiangsu         Mainland China
    ## 17                                       Jiangxi         Mainland China
    ## 18                               King County, WA                     US
    ## 19                                      Liaoning         Mainland China
    ## 20                                         Macau                  Macau
    ## 21                                       Ningxia         Mainland China
    ## 22                                      Shandong         Mainland China
    ## 23                                      Shanghai         Mainland China
    ## 24                                        Shanxi         Mainland China
    ## 25                                       Sichuan         Mainland China
    ## 26                                        Taiwan                 Taiwan
    ## 27                                       Tianjin         Mainland China
    ## 28                                        Yunnan         Mainland China
    ## 29                                      Zhejiang         Mainland China
    ## 30                                         Hubei         Mainland China
    ## 31                                         Hubei         Mainland China
    ## 32                                                                Japan
    ## 33                                                            Singapore
    ## 34                                                             Thailand
    ## 35                                                              Vietnam
    ## 36                                         Anhui         Mainland China
    ## 37                                       Beijing         Mainland China
    ## 38                                     Chongqing         Mainland China
    ## 39                                        Fujian         Mainland China
    ## 40                                         Gansu         Mainland China
    ## 41                                     Guangdong         Mainland China
    ## 42                                       Guangxi         Mainland China
    ## 43                                       Guizhou         Mainland China
    ## 44                                        Hainan         Mainland China
    ## 45                                  Heilongjiang         Mainland China
    ## 46                                     Hong Kong              Hong Kong
    ## 47                                         Hunan         Mainland China
    ## 48                                       Jiangsu         Mainland China
    ## 49                                       Jiangxi         Mainland China
    ## 50                                         Jilin         Mainland China
    ## 51                                      Liaoning         Mainland China
    ## 52                                         Macau                  Macau
    ## 53                                       Shaanxi         Mainland China
    ## 54                                      Shandong         Mainland China
    ## 55                                      Shanghai         Mainland China
    ## 56                                       Sichuan         Mainland China
    ## 57                                      Xinjiang         Mainland China
    ## 58                                        Yunnan         Mainland China
    ## 59                                      Zhejiang         Mainland China
    ## 60                                         Hebei         Mainland China
    ## 61                                     Guangdong         Mainland China
    ## 62                                                               France
    ## 63                                                                Japan
    ## 64                                                            Singapore
    ## 65                                                          South Korea
    ## 66                                                             Thailand
    ## 67                                         Anhui         Mainland China
    ## 68                                       Beijing         Mainland China
    ## 69                                     Chongqing         Mainland China
    ## 70                               Cook County, IL                     US
    ## 71                                        Fujian         Mainland China
    ## 72                                     Guangdong         Mainland China
    ## 73                                       Guangxi         Mainland China
    ## 74                                        Hainan         Mainland China
    ## 75                                         Hebei         Mainland China
    ## 76                                  Heilongjiang         Mainland China
    ## 77                                         Henan         Mainland China
    ## 78                                         Hubei         Mainland China
    ## 79                                         Hunan         Mainland China
    ## 80                                Inner Mongolia         Mainland China
    ## 81                                       Jiangsu         Mainland China
    ## 82                                       Jiangxi         Mainland China
    ## 83                                         Jilin         Mainland China
    ## 84                                      Liaoning         Mainland China
    ## 85                                       Ningxia         Mainland China
    ## 86                                       Shaanxi         Mainland China
    ## 87                                      Shandong         Mainland China
    ## 88                                      Shanghai         Mainland China
    ## 89                                       Sichuan         Mainland China
    ## 90                                        Taiwan                 Taiwan
    ## 91                                       Tianjin         Mainland China
    ## 92                                        Yunnan         Mainland China
    ## 93                                      Zhejiang         Mainland China
    ## 94                                  Heilongjiang         Mainland China
    ## 95                                         Hubei         Mainland China
    ## 96                                       Beijing         Mainland China
    ## 97                                         Hubei         Mainland China
    ## 98                                      Shanghai         Mainland China
    ## 99                                      Zhejiang         Mainland China
    ## 100                                                              France
    ## 101                                                            Malaysia
    ## 102                                                               Nepal
    ## 103                                                            Thailand
    ## 104                                        Anhui         Mainland China
    ## 105                                      Beijing         Mainland China
    ## 106                                    Chongqing         Mainland China
    ## 107                                       Fujian         Mainland China
    ## 108                                        Gansu         Mainland China
    ## 109                                    Guangdong         Mainland China
    ## 110                                      Guizhou         Mainland China
    ## 111                                       Hainan         Mainland China
    ## 112                                        Hebei         Mainland China
    ## 113                                 Heilongjiang         Mainland China
    ## 114                                        Henan         Mainland China
    ## 115                                    Hong Kong              Hong Kong
    ## 116                                        Hubei         Mainland China
    ## 117                                        Hunan         Mainland China
    ## 118                               Inner Mongolia         Mainland China
    ## 119                                      Jiangsu         Mainland China
    ## 120                                        Jilin         Mainland China
    ## 121                                     Liaoning         Mainland China
    ## 122                                      Ningxia         Mainland China
    ## 123                                      Qinghai         Mainland China
    ## 124                                      Shaanxi         Mainland China
    ## 125                                     Shandong         Mainland China
    ## 126                                     Shanghai         Mainland China
    ## 127                                       Shanxi         Mainland China
    ## 128                                      Sichuan         Mainland China
    ## 129                                      Tianjin         Mainland China
    ## 130                                     Xinjiang         Mainland China
    ## 131                                       Yunnan         Mainland China
    ## 132                                     Zhejiang         Mainland China
    ## 133                                        Hubei         Mainland China
    ## 134                                      Beijing         Mainland China
    ## 135                                        Hubei         Mainland China
    ## 136                                      Jiangsu         Mainland China
    ## 137                                                               Japan
    ## 138                                                            Malaysia
    ## 139                                                           Singapore
    ## 140                                                         South Korea
    ## 141                                                            Thailand
    ## 142                                        Anhui         Mainland China
    ## 143                                      Beijing         Mainland China
    ## 144                                    Chongqing         Mainland China
    ## 145                                       Fujian         Mainland China
    ## 146                                        Gansu         Mainland China
    ## 147                                    Guangdong         Mainland China
    ## 148                                      Guangxi         Mainland China
    ## 149                                      Guizhou         Mainland China
    ## 150                                       Hainan         Mainland China
    ## 151                                        Hebei         Mainland China
    ## 152                                 Heilongjiang         Mainland China
    ## 153                                        Henan         Mainland China
    ## 154                                    Hong Kong              Hong Kong
    ## 155                                        Hubei         Mainland China
    ## 156                                        Hunan         Mainland China
    ## 157                                      Jiangsu         Mainland China
    ## 158                                      Jiangxi         Mainland China
    ## 159                                     Liaoning         Mainland China
    ## 160                              Los Angeles, CA                     US
    ## 161                                        Macau                  Macau
    ## 162                              New South Wales              Australia
    ## 163                                      Ningxia         Mainland China
    ## 164                            Orange County, CA                     US
    ## 165                                      Shaanxi         Mainland China
    ## 166                                     Shandong         Mainland China
    ## 167                                     Shanghai         Mainland China
    ## 168                                       Shanxi         Mainland China
    ## 169                                      Sichuan         Mainland China
    ## 170                                       Taiwan                 Taiwan
    ## 171                                    Tempe, AZ                     US
    ## 172                                      Tianjin         Mainland China
    ## 173                                  Toronto, ON                 Canada
    ## 174                                     Victoria              Australia
    ## 175                                     Xinjiang         Mainland China
    ## 176                                       Yunnan         Mainland China
    ## 177                                     Zhejiang         Mainland China
    ## 178                                        Henan         Mainland China
    ## 179                                        Hubei         Mainland China
    ## 180                                     Shanghai         Mainland China
    ## 181                                                               Japan
    ## 182                                                            Thailand
    ## 183                                        Hubei         Mainland China
    ## 184                                                            Cambodia
    ## 185                                                             Germany
    ## 186                                                           Singapore
    ## 187                                                         South Korea
    ## 188                                                           Sri Lanka
    ## 189                                        Anhui         Mainland China
    ## 190                                      Beijing         Mainland China
    ## 191                                    Chongqing         Mainland China
    ## 192                                       Fujian         Mainland China
    ## 193                                        Gansu         Mainland China
    ## 194                                    Guangdong         Mainland China
    ## 195                                      Guangxi         Mainland China
    ## 196                                      Guizhou         Mainland China
    ## 197                                       Hainan         Mainland China
    ## 198                                        Hebei         Mainland China
    ## 199                                 Heilongjiang         Mainland China
    ## 200                                        Henan         Mainland China
    ## 201                                        Hubei         Mainland China
    ## 202                                        Hunan         Mainland China
    ## 203                               Inner Mongolia         Mainland China
    ## 204                                      Jiangsu         Mainland China
    ## 205                                      Jiangxi         Mainland China
    ## 206                                        Jilin         Mainland China
    ## 207                                     Liaoning         Mainland China
    ## 208                                        Macau                  Macau
    ## 209                              New South Wales              Australia
    ## 210                                      Ningxia         Mainland China
    ## 211                                      Qinghai         Mainland China
    ## 212                                      Shaanxi         Mainland China
    ## 213                                     Shandong         Mainland China
    ## 214                                     Shanghai         Mainland China
    ## 215                                       Shanxi         Mainland China
    ## 216                                      Sichuan         Mainland China
    ## 217                                       Taiwan                 Taiwan
    ## 218                                      Tianjin         Mainland China
    ## 219                                     Xinjiang         Mainland China
    ## 220                                       Yunnan         Mainland China
    ## 221                                     Zhejiang         Mainland China
    ## 222                                      Beijing         Mainland China
    ## 223                                       Hainan         Mainland China
    ## 224                                        Hubei         Mainland China
    ## 225                                    Guangdong         Mainland China
    ## 226                                        Hubei         Mainland China
    ## 227                                      Jiangxi         Mainland China
    ## 228                                     Shanghai         Mainland China
    ## 229                                                              France
    ## 230                                                             Germany
    ## 231                                                               Japan
    ## 232                                                           Singapore
    ## 233                                                            Thailand
    ## 234                                        Anhui         Mainland China
    ## 235                                      Beijing         Mainland China
    ## 236                             British Columbia                 Canada
    ## 237                                    Chongqing         Mainland China
    ## 238                                       Fujian         Mainland China
    ## 239                                        Gansu         Mainland China
    ## 240                                    Guangdong         Mainland China
    ## 241                                      Guangxi         Mainland China
    ## 242                                      Guizhou         Mainland China
    ## 243                                       Hainan         Mainland China
    ## 244                                        Hebei         Mainland China
    ## 245                                 Heilongjiang         Mainland China
    ## 246                                        Henan         Mainland China
    ## 247                                        Hubei         Mainland China
    ## 248                                        Hunan         Mainland China
    ## 249                               Inner Mongolia         Mainland China
    ## 250                                      Jiangsu         Mainland China
    ## 251                                      Jiangxi         Mainland China
    ## 252                                        Jilin         Mainland China
    ## 253                                     Liaoning         Mainland China
    ## 254                                        Macau                  Macau
    ## 255                                      Ningxia         Mainland China
    ## 256                                      Shaanxi         Mainland China
    ## 257                                     Shandong         Mainland China
    ## 258                                     Shanghai         Mainland China
    ## 259                                       Shanxi         Mainland China
    ## 260                                      Sichuan         Mainland China
    ## 261                                       Taiwan                 Taiwan
    ## 262                                      Tianjin         Mainland China
    ## 263                                     Xinjiang         Mainland China
    ## 264                                       Yunnan         Mainland China
    ## 265                                     Zhejiang         Mainland China
    ## 266                                        Hubei         Mainland China
    ## 267                                                            Thailand
    ## 268                                      Beijing         Mainland China
    ## 269                                      Guangxi         Mainland China
    ## 270                                        Hubei         Mainland China
    ## 271                                      Jiangxi         Mainland China
    ## 272                                     Shanghai         Mainland China
    ## 273                                     Zhejiang         Mainland China
    ## 274                                                             Finland
    ## 275                                                              France
    ## 276                                                            Malaysia
    ## 277                                                United Arab Emirates
    ## 278                                        Anhui         Mainland China
    ## 279                                      Beijing         Mainland China
    ## 280                                    Chongqing         Mainland China
    ## 281                                       Fujian         Mainland China
    ## 282                                        Gansu         Mainland China
    ## 283                                    Guangdong         Mainland China
    ## 284                                      Guangxi         Mainland China
    ## 285                                       Hainan         Mainland China
    ## 286                                        Hebei         Mainland China
    ## 287                                 Heilongjiang         Mainland China
    ## 288                                        Henan         Mainland China
    ## 289                                    Hong Kong              Hong Kong
    ## 290                                        Hunan         Mainland China
    ## 291                               Inner Mongolia         Mainland China
    ## 292                                      Jiangsu         Mainland China
    ## 293                                        Jilin         Mainland China
    ## 294                                     Liaoning         Mainland China
    ## 295                                      Ningxia         Mainland China
    ## 296                                   Queensland              Australia
    ## 297                                      Shaanxi         Mainland China
    ## 298                                     Shandong         Mainland China
    ## 299                                     Shanghai         Mainland China
    ## 300                                      Sichuan         Mainland China
    ## 301                                      Tianjin         Mainland China
    ## 302                                     Xinjiang         Mainland China
    ## 303                                       Yunnan         Mainland China
    ## 304                                     Zhejiang         Mainland China
    ## 305                                        Henan         Mainland China
    ## 306                                      Sichuan         Mainland China
    ## 307                                        Anhui         Mainland China
    ## 308                                    Chongqing         Mainland China
    ## 309                                    Guangdong         Mainland China
    ## 310                                      Guizhou         Mainland China
    ## 311                                        Henan         Mainland China
    ## 312                                        Hubei         Mainland China
    ## 313                                     Liaoning         Mainland China
    ## 314                                     Shandong         Mainland China
    ## 315                                     Shanghai         Mainland China
    ## 316                                       Shanxi         Mainland China
    ## 317                                      Sichuan         Mainland China
    ## 318                                                               India
    ## 319                                                               Japan
    ## 320                                                            Malaysia
    ## 321                                                         Philippines
    ## 322                                                           Singapore
    ## 323                                        Anhui         Mainland China
    ## 324                                      Beijing         Mainland China
    ## 325                                    Chongqing         Mainland China
    ## 326                                       Fujian         Mainland China
    ## 327                                        Gansu         Mainland China
    ## 328                                    Guangdong         Mainland China
    ## 329                                      Guangxi         Mainland China
    ## 330                                      Guizhou         Mainland China
    ## 331                                       Hainan         Mainland China
    ## 332                                        Hebei         Mainland China
    ## 333                                 Heilongjiang         Mainland China
    ## 334                                        Henan         Mainland China
    ## 335                                        Hubei         Mainland China
    ## 336                                        Hunan         Mainland China
    ## 337                               Inner Mongolia         Mainland China
    ## 338                                      Jiangsu         Mainland China
    ## 339                                      Jiangxi         Mainland China
    ## 340                                        Jilin         Mainland China
    ## 341                                     Liaoning         Mainland China
    ## 342                                      Ningxia         Mainland China
    ## 343                                      Qinghai         Mainland China
    ## 344                                   Queensland              Australia
    ## 345                                      Shaanxi         Mainland China
    ## 346                                     Shandong         Mainland China
    ## 347                                     Shanghai         Mainland China
    ## 348                                       Shanxi         Mainland China
    ## 349                                      Sichuan         Mainland China
    ## 350                                       Taiwan                 Taiwan
    ## 351                                      Tianjin         Mainland China
    ## 352                                        Tibet         Mainland China
    ## 353                                     Victoria              Australia
    ## 354                                     Xinjiang         Mainland China
    ## 355                                       Yunnan         Mainland China
    ## 356                                     Zhejiang         Mainland China
    ## 357                                 Heilongjiang         Mainland China
    ## 358                                        Hubei         Mainland China
    ## 359                                    Guangdong         Mainland China
    ## 360                                       Hainan         Mainland China
    ## 361                                        Henan         Mainland China
    ## 362                                        Hubei         Mainland China
    ## 363                                        Hunan         Mainland China
    ## 364                                      Jiangxi         Mainland China
    ## 365                                        Jilin         Mainland China
    ## 366                              New South Wales              Australia
    ## 367                                     Zhejiang         Mainland China
    ## 368                                                             Germany
    ## 369                                                               Italy
    ## 370                                                               Japan
    ## 371                                                              Russia
    ## 372                                                           Singapore
    ## 373                                                         South Korea
    ## 374                                                              Sweden
    ## 375                                                            Thailand
    ## 376                                                                  UK
    ## 377                                        Anhui         Mainland China
    ## 378                                      Beijing         Mainland China
    ## 379                                    Chongqing         Mainland China
    ## 380                              Cook County, IL                     US
    ## 381                                       Fujian         Mainland China
    ## 382                                        Gansu         Mainland China
    ## 383                                    Guangdong         Mainland China
    ## 384                                      Guangxi         Mainland China
    ## 385                                      Guizhou         Mainland China
    ## 386                                       Hainan         Mainland China
    ## 387                                        Hebei         Mainland China
    ## 388                                 Heilongjiang         Mainland China
    ## 389                                        Henan         Mainland China
    ## 390                                    Hong Kong              Hong Kong
    ## 391                                        Hubei         Mainland China
    ## 392                                        Hunan         Mainland China
    ## 393                               Inner Mongolia         Mainland China
    ## 394                                      Jiangsu         Mainland China
    ## 395                                      Jiangxi         Mainland China
    ## 396                                     Liaoning         Mainland China
    ## 397                                   London, ON                 Canada
    ## 398                                      Ningxia         Mainland China
    ## 399                                   Queensland              Australia
    ## 400                              Santa Clara, CA                     US
    ## 401                                      Shaanxi         Mainland China
    ## 402                                     Shandong         Mainland China
    ## 403                                     Shanghai         Mainland China
    ## 404                                       Shanxi         Mainland China
    ## 405                                      Sichuan         Mainland China
    ## 406                                       Taiwan                 Taiwan
    ## 407                                      Tianjin         Mainland China
    ## 408                                  Toronto, ON                 Canada
    ## 409                                     Victoria              Australia
    ## 410                                     Xinjiang         Mainland China
    ## 411                                       Yunnan         Mainland China
    ## 412                                     Zhejiang         Mainland China
    ## 413                                        Hubei         Mainland China
    ## 414                                        Anhui         Mainland China
    ## 415                                      Beijing         Mainland China
    ## 416                                    Guangdong         Mainland China
    ## 417                                      Guizhou         Mainland China
    ## 418                                        Henan         Mainland China
    ## 419                                        Hubei         Mainland China
    ## 420                               Inner Mongolia         Mainland China
    ## 421                                      Jiangsu         Mainland China
    ## 422                                      Jiangxi         Mainland China
    ## 423                                     Shandong         Mainland China
    ## 424                                     Shanghai         Mainland China
    ## 425                                       Yunnan         Mainland China
    ## 426                                     Zhejiang         Mainland China
    ## 427                                                              France
    ## 428                                                             Germany
    ## 429                                                               Japan
    ## 430                                                           Singapore
    ## 431                                                         South Korea
    ## 432                                                               Spain
    ## 433                                                             Vietnam
    ## 434                                        Anhui         Mainland China
    ## 435                                      Beijing         Mainland China
    ## 436                                   Boston, MA                     US
    ## 437                                    Chongqing         Mainland China
    ## 438                                       Fujian         Mainland China
    ## 439                                        Gansu         Mainland China
    ## 440                                    Guangdong         Mainland China
    ## 441                                      Guangxi         Mainland China
    ## 442                                       Hainan         Mainland China
    ## 443                                        Hebei         Mainland China
    ## 444                                 Heilongjiang         Mainland China
    ## 445                                        Henan         Mainland China
    ## 446                                    Hong Kong              Hong Kong
    ## 447                                        Hubei         Mainland China
    ## 448                                        Hunan         Mainland China
    ## 449                               Inner Mongolia         Mainland China
    ## 450                                      Jiangsu         Mainland China
    ## 451                                      Jiangxi         Mainland China
    ## 452                                        Jilin         Mainland China
    ## 453                                     Liaoning         Mainland China
    ## 454                                      Ningxia         Mainland China
    ## 455                                      Qinghai         Mainland China
    ## 456                                   Queensland              Australia
    ## 457                                      Shaanxi         Mainland China
    ## 458                                     Shandong         Mainland China
    ## 459                                     Shanghai         Mainland China
    ## 460                                       Shanxi         Mainland China
    ## 461                                      Sichuan         Mainland China
    ## 462                              South Australia              Australia
    ## 463                                      Tianjin         Mainland China
    ## 464                                     Victoria              Australia
    ## 465                                     Xinjiang         Mainland China
    ## 466                                       Yunnan         Mainland China
    ## 467                                     Zhejiang         Mainland China
    ## 468                                    Chongqing         Mainland China
    ## 469                                        Hubei         Mainland China
    ## 470                                                             Vietnam
    ## 471                                        Anhui         Mainland China
    ## 472                                      Beijing         Mainland China
    ## 473                                    Chongqing         Mainland China
    ## 474                                    Guangdong         Mainland China
    ## 475                                 Heilongjiang         Mainland China
    ## 476                                        Hubei         Mainland China
    ## 477                                        Hunan         Mainland China
    ## 478                                      Jiangsu         Mainland China
    ## 479                                      Jiangxi         Mainland China
    ## 480                                     Shandong         Mainland China
    ## 481                                     Shanghai         Mainland China
    ## 482                                      Sichuan         Mainland China
    ## 483                                       Yunnan         Mainland China
    ## 484                                     Zhejiang         Mainland China
    ## 485                                                             Germany
    ## 486                                                               India
    ## 487                                                         Philippines
    ## 488                                                           Singapore
    ## 489                                                         South Korea
    ## 490                                                United Arab Emirates
    ## 491                                        Anhui         Mainland China
    ## 492                                      Beijing         Mainland China
    ## 493                                    Chongqing         Mainland China
    ## 494                                       Fujian         Mainland China
    ## 495                                        Gansu         Mainland China
    ## 496                                    Guangdong         Mainland China
    ## 497                                      Guangxi         Mainland China
    ## 498                                      Guizhou         Mainland China
    ## 499                                       Hainan         Mainland China
    ## 500                                        Hebei         Mainland China
    ## 501                                 Heilongjiang         Mainland China
    ## 502                                        Henan         Mainland China
    ## 503                                    Hong Kong              Hong Kong
    ## 504                                        Hubei         Mainland China
    ## 505                                        Hunan         Mainland China
    ## 506                               Inner Mongolia         Mainland China
    ## 507                                      Jiangsu         Mainland China
    ## 508                                      Jiangxi         Mainland China
    ## 509                                        Jilin         Mainland China
    ## 510                                     Liaoning         Mainland China
    ## 511                                        Macau                  Macau
    ## 512                                      Ningxia         Mainland China
    ## 513                                      Qinghai         Mainland China
    ## 514                                   Queensland              Australia
    ## 515                                      Shaanxi         Mainland China
    ## 516                                     Shandong         Mainland China
    ## 517                                     Shanghai         Mainland China
    ## 518                                       Shanxi         Mainland China
    ## 519                                      Sichuan         Mainland China
    ## 520                              South Australia              Australia
    ## 521                                      Tianjin         Mainland China
    ## 522                                     Xinjiang         Mainland China
    ## 523                                       Yunnan         Mainland China
    ## 524                                     Zhejiang         Mainland China
    ## 525                                                         Philippines
    ## 526                                    Chongqing         Mainland China
    ## 527                                        Hubei         Mainland China
    ## 528                                        Anhui         Mainland China
    ## 529                                    Chongqing         Mainland China
    ## 530                                        Gansu         Mainland China
    ## 531                                    Guangdong         Mainland China
    ## 532                                       Hainan         Mainland China
    ## 533                                        Hebei         Mainland China
    ## 534                                        Henan         Mainland China
    ## 535                                        Hubei         Mainland China
    ## 536                                        Hunan         Mainland China
    ## 537                                      Jiangsu         Mainland China
    ## 538                                      Jiangxi         Mainland China
    ## 539                                     Shandong         Mainland China
    ## 540                                       Shanxi         Mainland China
    ## 541                                      Sichuan         Mainland China
    ## 542                                      Tianjin         Mainland China
    ## 543                                       Yunnan         Mainland China
    ## 544                                     Zhejiang         Mainland China
    ## 545                                                             Germany
    ## 546                                                               India
    ## 547                                                             Vietnam
    ## 548                                        Anhui         Mainland China
    ## 549                                      Beijing         Mainland China
    ## 550                                    Chongqing         Mainland China
    ## 551                                       Fujian         Mainland China
    ## 552                                        Gansu         Mainland China
    ## 553                                    Guangdong         Mainland China
    ## 554                                      Guangxi         Mainland China
    ## 555                                      Guizhou         Mainland China
    ## 556                                       Hainan         Mainland China
    ## 557                                        Hebei         Mainland China
    ## 558                                 Heilongjiang         Mainland China
    ## 559                                        Henan         Mainland China
    ## 560                                        Hubei         Mainland China
    ## 561                                        Hunan         Mainland China
    ## 562                               Inner Mongolia         Mainland China
    ## 563                                      Jiangsu         Mainland China
    ## 564                                      Jiangxi         Mainland China
    ## 565                                        Jilin         Mainland China
    ## 566                                     Liaoning         Mainland China
    ## 567                                      Ningxia         Mainland China
    ## 568                                      Qinghai         Mainland China
    ## 569                               San Benito, CA                     US
    ## 570                              Santa Clara, CA                     US
    ## 571                                      Shaanxi         Mainland China
    ## 572                                     Shandong         Mainland China
    ## 573                                     Shanghai         Mainland China
    ## 574                                       Shanxi         Mainland China
    ## 575                                      Sichuan         Mainland China
    ## 576                                      Tianjin         Mainland China
    ## 577                                     Xinjiang         Mainland China
    ## 578                                       Yunnan         Mainland China
    ## 579                                     Zhejiang         Mainland China
    ## 580                                        Hubei         Mainland China
    ## 581                                        Anhui         Mainland China
    ## 582                                      Beijing         Mainland China
    ## 583                                    Chongqing         Mainland China
    ## 584                                       Fujian         Mainland China
    ## 585                                    Guangdong         Mainland China
    ## 586                                      Guangxi         Mainland China
    ## 587                                        Henan         Mainland China
    ## 588                                        Hubei         Mainland China
    ## 589                                        Hunan         Mainland China
    ## 590                                      Jiangsu         Mainland China
    ## 591                                      Jiangxi         Mainland China
    ## 592                                      Ningxia         Mainland China
    ## 593                                     Shandong         Mainland China
    ## 594                                       Shanxi         Mainland China
    ## 595                                      Sichuan         Mainland China
    ## 596                                       Yunnan         Mainland China
    ## 597                                     Zhejiang         Mainland China
    ## 598                                                             Belgium
    ## 599                                                               Japan
    ## 600                                                            Malaysia
    ## 601                                                           Singapore
    ## 602                                                         South Korea
    ## 603                                                            Thailand
    ## 604                                        Anhui         Mainland China
    ## 605                                      Beijing         Mainland China
    ## 606                                    Chongqing         Mainland China
    ## 607                                       Fujian         Mainland China
    ## 608                                        Gansu         Mainland China
    ## 609                                    Guangdong         Mainland China
    ## 610                                      Guangxi         Mainland China
    ## 611                                      Guizhou         Mainland China
    ## 612                                       Hainan         Mainland China
    ## 613                                        Hebei         Mainland China
    ## 614                                 Heilongjiang         Mainland China
    ## 615                                        Henan         Mainland China
    ## 616                                    Hong Kong              Hong Kong
    ## 617                                        Hubei         Mainland China
    ## 618                                        Hunan         Mainland China
    ## 619                               Inner Mongolia         Mainland China
    ## 620                                      Jiangsu         Mainland China
    ## 621                                      Jiangxi         Mainland China
    ## 622                                        Jilin         Mainland China
    ## 623                                     Liaoning         Mainland China
    ## 624                                        Macau                  Macau
    ## 625                                      Ningxia         Mainland China
    ## 626                                      Qinghai         Mainland China
    ## 627                                   Queensland              Australia
    ## 628                                      Shaanxi         Mainland China
    ## 629                                     Shandong         Mainland China
    ## 630                                     Shanghai         Mainland China
    ## 631                                       Shanxi         Mainland China
    ## 632                                      Sichuan         Mainland China
    ## 633                                       Taiwan                 Taiwan
    ## 634                                      Tianjin         Mainland China
    ## 635                                     Xinjiang         Mainland China
    ## 636                                       Yunnan         Mainland China
    ## 637                                     Zhejiang         Mainland China
    ## 638                                    Hong Kong              Hong Kong
    ## 639                                        Hubei         Mainland China
    ## 640                                        Anhui         Mainland China
    ## 641                                      Beijing         Mainland China
    ## 642                                       Fujian         Mainland China
    ## 643                                        Gansu         Mainland China
    ## 644                                    Guangdong         Mainland China
    ## 645                                      Guangxi         Mainland China
    ## 646                                       Hainan         Mainland China
    ## 647                                        Hebei         Mainland China
    ## 648                                 Heilongjiang         Mainland China
    ## 649                                        Henan         Mainland China
    ## 650                                        Hubei         Mainland China
    ## 651                                        Hunan         Mainland China
    ## 652                                      Jiangsu         Mainland China
    ## 653                                      Jiangxi         Mainland China
    ## 654                                     Liaoning         Mainland China
    ## 655                                      Shaanxi         Mainland China
    ## 656                                     Shandong         Mainland China
    ## 657                                     Shanghai         Mainland China
    ## 658                                       Shanxi         Mainland China
    ## 659                                      Tianjin         Mainland China
    ## 660                                     Zhejiang         Mainland China
    ## 661                                                            Malaysia
    ## 662                                                           Singapore
    ## 663                                                         South Korea
    ## 664                                        Anhui         Mainland China
    ## 665                                      Beijing         Mainland China
    ## 666                             British Columbia                 Canada
    ## 667                                    Chongqing         Mainland China
    ## 668                                       Fujian         Mainland China
    ## 669                                        Gansu         Mainland China
    ## 670                                    Guangdong         Mainland China
    ## 671                                      Guangxi         Mainland China
    ## 672                                      Guizhou         Mainland China
    ## 673                                       Hainan         Mainland China
    ## 674                                        Hebei         Mainland China
    ## 675                                 Heilongjiang         Mainland China
    ## 676                                        Henan         Mainland China
    ## 677                                    Hong Kong              Hong Kong
    ## 678                                        Hubei         Mainland China
    ## 679                                        Hunan         Mainland China
    ## 680                               Inner Mongolia         Mainland China
    ## 681                                      Jiangsu         Mainland China
    ## 682                                      Jiangxi         Mainland China
    ## 683                                        Jilin         Mainland China
    ## 684                                     Liaoning         Mainland China
    ## 685                                  Madison, WI                     US
    ## 686                                      Qinghai         Mainland China
    ## 687                                      Shaanxi         Mainland China
    ## 688                                     Shandong         Mainland China
    ## 689                                     Shanghai         Mainland China
    ## 690                                      Sichuan         Mainland China
    ## 691                                      Tianjin         Mainland China
    ## 692                                     Xinjiang         Mainland China
    ## 693                                       Yunnan         Mainland China
    ## 694                                     Zhejiang         Mainland China
    ## 695                                      Guizhou         Mainland China
    ## 696                                        Hubei         Mainland China
    ## 697                                      Tianjin         Mainland China
    ## 698                                        Anhui         Mainland China
    ## 699                                      Beijing         Mainland China
    ## 700                                    Chongqing         Mainland China
    ## 701                                       Fujian         Mainland China
    ## 702                                        Gansu         Mainland China
    ## 703                                    Guangdong         Mainland China
    ## 704                                      Guangxi         Mainland China
    ## 705                                      Guizhou         Mainland China
    ## 706                                        Hebei         Mainland China
    ## 707                                 Heilongjiang         Mainland China
    ## 708                                        Henan         Mainland China
    ## 709                                        Hubei         Mainland China
    ## 710                                        Hunan         Mainland China
    ## 711                               Inner Mongolia         Mainland China
    ## 712                                      Jiangsu         Mainland China
    ## 713                                      Jiangxi         Mainland China
    ## 714                                        Jilin         Mainland China
    ## 715                                     Liaoning         Mainland China
    ## 716                                      Qinghai         Mainland China
    ## 717                                      Shaanxi         Mainland China
    ## 718                                     Shandong         Mainland China
    ## 719                                     Shanghai         Mainland China
    ## 720                                       Shanxi         Mainland China
    ## 721                                      Sichuan         Mainland China
    ## 722                                     Zhejiang         Mainland China
    ## 723                                                               Japan
    ## 724                                                         South Korea
    ## 725                                                             Vietnam
    ## 726                                        Anhui         Mainland China
    ## 727                                      Beijing         Mainland China
    ## 728                                    Chongqing         Mainland China
    ## 729                                       Fujian         Mainland China
    ## 730                                    Guangdong         Mainland China
    ## 731                                      Guangxi         Mainland China
    ## 732                                      Guizhou         Mainland China
    ## 733                                       Hainan         Mainland China
    ## 734                                        Hebei         Mainland China
    ## 735                                 Heilongjiang         Mainland China
    ## 736                                        Henan         Mainland China
    ## 737                                    Hong Kong              Hong Kong
    ## 738                                        Hubei         Mainland China
    ## 739                                        Hunan         Mainland China
    ## 740                               Inner Mongolia         Mainland China
    ## 741                                      Jiangsu         Mainland China
    ## 742                                      Jiangxi         Mainland China
    ## 743                                        Jilin         Mainland China
    ## 744                                     Liaoning         Mainland China
    ## 745                                      Ningxia         Mainland China
    ## 746                                      Qinghai         Mainland China
    ## 747                                   Queensland              Australia
    ## 748                                      Shaanxi         Mainland China
    ## 749                                     Shandong         Mainland China
    ## 750                                     Shanghai         Mainland China
    ## 751                                       Shanxi         Mainland China
    ## 752                                      Sichuan         Mainland China
    ## 753                                       Taiwan                 Taiwan
    ## 754                                      Tianjin         Mainland China
    ## 755                                     Xinjiang         Mainland China
    ## 756                                       Yunnan         Mainland China
    ## 757                                     Zhejiang         Mainland China
    ## 758                                 Heilongjiang         Mainland China
    ## 759                                        Hubei         Mainland China
    ## 760                                        Anhui         Mainland China
    ## 761                                      Beijing         Mainland China
    ## 762                                    Chongqing         Mainland China
    ## 763                                       Fujian         Mainland China
    ## 764                                    Guangdong         Mainland China
    ## 765                                      Guangxi         Mainland China
    ## 766                                      Guizhou         Mainland China
    ## 767                                       Hainan         Mainland China
    ## 768                                        Hebei         Mainland China
    ## 769                                 Heilongjiang         Mainland China
    ## 770                                        Henan         Mainland China
    ## 771                                        Hubei         Mainland China
    ## 772                                        Hunan         Mainland China
    ## 773                               Inner Mongolia         Mainland China
    ## 774                                      Jiangsu         Mainland China
    ## 775                                      Jiangxi         Mainland China
    ## 776                                        Jilin         Mainland China
    ## 777                                     Liaoning         Mainland China
    ## 778                                        Macau                  Macau
    ## 779                                      Shaanxi         Mainland China
    ## 780                                     Shandong         Mainland China
    ## 781                                     Shanghai         Mainland China
    ## 782                                       Shanxi         Mainland China
    ## 783                                      Sichuan         Mainland China
    ## 784                                       Taiwan                 Taiwan
    ## 785                                       Yunnan         Mainland China
    ## 786                                     Zhejiang         Mainland China
    ## 787                                                             Germany
    ## 788                                                               Italy
    ## 789                                                               Japan
    ## 790                                                         Philippines
    ## 791                                                           Singapore
    ## 792                                                         South Korea
    ## 793                                                                  UK
    ## 794                                        Anhui         Mainland China
    ## 795                                      Beijing         Mainland China
    ## 796                             British Columbia                 Canada
    ## 797                                    Chongqing         Mainland China
    ## 798                 Diamond Princess cruise ship                 Others
    ## 799                                       Fujian         Mainland China
    ## 800                                        Gansu         Mainland China
    ## 801                                    Guangdong         Mainland China
    ## 802                                      Guangxi         Mainland China
    ## 803                                      Guizhou         Mainland China
    ## 804                                       Hainan         Mainland China
    ## 805                                        Hebei         Mainland China
    ## 806                                 Heilongjiang         Mainland China
    ## 807                                        Henan         Mainland China
    ## 808                                    Hong Kong              Hong Kong
    ## 809                                        Hubei         Mainland China
    ## 810                                        Hunan         Mainland China
    ## 811                               Inner Mongolia         Mainland China
    ## 812                                      Jiangsu         Mainland China
    ## 813                                      Jiangxi         Mainland China
    ## 814                                        Jilin         Mainland China
    ## 815                                     Liaoning         Mainland China
    ## 816                                      Ningxia         Mainland China
    ## 817                                   Queensland              Australia
    ## 818                                      Shaanxi         Mainland China
    ## 819                                     Shandong         Mainland China
    ## 820                                     Shanghai         Mainland China
    ## 821                                       Shanxi         Mainland China
    ## 822                                      Sichuan         Mainland China
    ## 823                                      Tianjin         Mainland China
    ## 824                                     Xinjiang         Mainland China
    ## 825                                       Yunnan         Mainland China
    ## 826                                     Zhejiang         Mainland China
    ## 827                                    Guangdong         Mainland China
    ## 828                                       Hainan         Mainland China
    ## 829                                        Henan         Mainland China
    ## 830                                        Hubei         Mainland China
    ## 831                                        Jilin         Mainland China
    ## 832                                                            Malaysia
    ## 833                                                         South Korea
    ## 834                                        Anhui         Mainland China
    ## 835                                      Beijing         Mainland China
    ## 836                                    Chongqing         Mainland China
    ## 837                                       Fujian         Mainland China
    ## 838                                        Gansu         Mainland China
    ## 839                                    Guangdong         Mainland China
    ## 840                                      Guangxi         Mainland China
    ## 841                                       Hainan         Mainland China
    ## 842                                        Hebei         Mainland China
    ## 843                                 Heilongjiang         Mainland China
    ## 844                                        Henan         Mainland China
    ## 845                                        Hubei         Mainland China
    ## 846                                        Hunan         Mainland China
    ## 847                               Inner Mongolia         Mainland China
    ## 848                                      Jiangsu         Mainland China
    ## 849                                      Jiangxi         Mainland China
    ## 850                                     Liaoning         Mainland China
    ## 851                                      Ningxia         Mainland China
    ## 852                                      Shaanxi         Mainland China
    ## 853                                     Shandong         Mainland China
    ## 854                                     Shanghai         Mainland China
    ## 855                                       Shanxi         Mainland China
    ## 856                                      Sichuan         Mainland China
    ## 857                                       Yunnan         Mainland China
    ## 858                                     Zhejiang         Mainland China
    ## 859                                                              France
    ## 860                                                            Malaysia
    ## 861                                                           Singapore
    ## 862                                                            Thailand
    ## 863                                                United Arab Emirates
    ## 864                                                             Vietnam
    ## 865                                        Anhui         Mainland China
    ## 866                                      Beijing         Mainland China
    ## 867                                    Chongqing         Mainland China
    ## 868                                       Fujian         Mainland China
    ## 869                                        Gansu         Mainland China
    ## 870                                    Guangdong         Mainland China
    ## 871                                      Guangxi         Mainland China
    ## 872                                      Guizhou         Mainland China
    ## 873                                       Hainan         Mainland China
    ## 874                                        Hebei         Mainland China
    ## 875                                 Heilongjiang         Mainland China
    ## 876                                        Henan         Mainland China
    ## 877                                    Hong Kong              Hong Kong
    ## 878                                        Hubei         Mainland China
    ## 879                                        Hunan         Mainland China
    ## 880                               Inner Mongolia         Mainland China
    ## 881                                      Jiangsu         Mainland China
    ## 882                                      Jiangxi         Mainland China
    ## 883                                        Jilin         Mainland China
    ## 884                                     Liaoning         Mainland China
    ## 885                                      Ningxia         Mainland China
    ## 886                                      Shaanxi         Mainland China
    ## 887                                     Shandong         Mainland China
    ## 888                                     Shanghai         Mainland China
    ## 889                                       Shanxi         Mainland China
    ## 890                                      Sichuan         Mainland China
    ## 891                                       Taiwan                 Taiwan
    ## 892                                      Tianjin         Mainland China
    ## 893                                     Xinjiang         Mainland China
    ## 894                                     Zhejiang         Mainland China
    ## 895                                      Beijing         Mainland China
    ## 896                                        Gansu         Mainland China
    ## 897                                 Heilongjiang         Mainland China
    ## 898                                        Henan         Mainland China
    ## 899                                        Hubei         Mainland China
    ## 900                                        Hunan         Mainland China
    ## 901                                                           Singapore
    ## 902                                                           Sri Lanka
    ## 903                                                            Thailand
    ## 904                                        Anhui         Mainland China
    ## 905                                      Beijing         Mainland China
    ## 906                                    Chongqing         Mainland China
    ## 907                                       Fujian         Mainland China
    ## 908                                        Gansu         Mainland China
    ## 909                                    Guangdong         Mainland China
    ## 910                                      Guizhou         Mainland China
    ## 911                                       Hainan         Mainland China
    ## 912                                        Hebei         Mainland China
    ## 913                                 Heilongjiang         Mainland China
    ## 914                                        Henan         Mainland China
    ## 915                                        Hubei         Mainland China
    ## 916                                        Hunan         Mainland China
    ## 917                                      Jiangsu         Mainland China
    ## 918                                      Jiangxi         Mainland China
    ## 919                                     Liaoning         Mainland China
    ## 920                                      Ningxia         Mainland China
    ## 921                                      Shaanxi         Mainland China
    ## 922                                     Shandong         Mainland China
    ## 923                                     Shanghai         Mainland China
    ## 924                                       Shanxi         Mainland China
    ## 925                                      Sichuan         Mainland China
    ## 926                                      Tianjin         Mainland China
    ## 927                                       Yunnan         Mainland China
    ## 928                                     Zhejiang         Mainland China
    ## 929                                                             Germany
    ## 930                                                               Japan
    ## 931                                                           Singapore
    ## 932                                                         South Korea
    ## 933                                                               Spain
    ## 934                                        Anhui         Mainland China
    ## 935                                      Beijing         Mainland China
    ## 936                                    Chongqing         Mainland China
    ## 937                 Diamond Princess cruise ship                 Others
    ## 938                                       Fujian         Mainland China
    ## 939                                        Gansu         Mainland China
    ## 940                                    Guangdong         Mainland China
    ## 941                                      Guangxi         Mainland China
    ## 942                                      Guizhou         Mainland China
    ## 943                                       Hainan         Mainland China
    ## 944                                        Hebei         Mainland China
    ## 945                                 Heilongjiang         Mainland China
    ## 946                                        Henan         Mainland China
    ## 947                                    Hong Kong              Hong Kong
    ## 948                                        Hubei         Mainland China
    ## 949                                        Hunan         Mainland China
    ## 950                               Inner Mongolia         Mainland China
    ## 951                                      Jiangsu         Mainland China
    ## 952                                      Jiangxi         Mainland China
    ## 953                                        Jilin         Mainland China
    ## 954                                     Liaoning         Mainland China
    ## 955                                      Shaanxi         Mainland China
    ## 956                                     Shandong         Mainland China
    ## 957                                     Shanghai         Mainland China
    ## 958                                       Shanxi         Mainland China
    ## 959                                      Sichuan         Mainland China
    ## 960                                       Taiwan                 Taiwan
    ## 961                                      Tianjin         Mainland China
    ## 962                                     Xinjiang         Mainland China
    ## 963                                       Yunnan         Mainland China
    ## 964                                     Zhejiang         Mainland China
    ## 965                                        Anhui         Mainland China
    ## 966                                        Gansu         Mainland China
    ## 967                                      Guangxi         Mainland China
    ## 968                                       Hainan         Mainland China
    ## 969                                        Hebei         Mainland China
    ## 970                                 Heilongjiang         Mainland China
    ## 971                                        Henan         Mainland China
    ## 972                                        Hubei         Mainland China
    ## 973                                     Shandong         Mainland China
    ## 974                                                         South Korea
    ## 975                                        Anhui         Mainland China
    ## 976                                      Beijing         Mainland China
    ## 977                                    Chongqing         Mainland China
    ## 978                              Cook County, IL                     US
    ## 979                                       Fujian         Mainland China
    ## 980                                        Gansu         Mainland China
    ## 981                                    Guangdong         Mainland China
    ## 982                                      Guangxi         Mainland China
    ## 983                                       Hainan         Mainland China
    ## 984                                        Hebei         Mainland China
    ## 985                                 Heilongjiang         Mainland China
    ## 986                                        Henan         Mainland China
    ## 987                                        Hubei         Mainland China
    ## 988                                        Hunan         Mainland China
    ## 989                                      Jiangsu         Mainland China
    ## 990                                      Jiangxi         Mainland China
    ## 991                                        Jilin         Mainland China
    ## 992                              King County, WA                     US
    ## 993                                     Liaoning         Mainland China
    ## 994                                      Ningxia         Mainland China
    ## 995                                      Shaanxi         Mainland China
    ## 996                                     Shandong         Mainland China
    ## 997                                     Shanghai         Mainland China
    ## 998                                       Shanxi         Mainland China
    ## 999                                      Sichuan         Mainland China
    ## 1000                                      Yunnan         Mainland China
    ## 1001                                    Zhejiang         Mainland China
    ## 1002                                                           Malaysia
    ## 1003                                                          Singapore
    ## 1004                                                        South Korea
    ## 1005                                                                 UK
    ## 1006                                               United Arab Emirates
    ## 1007                                                            Vietnam
    ## 1008                                       Anhui         Mainland China
    ## 1009                                     Beijing         Mainland China
    ## 1010                                   Chongqing         Mainland China
    ## 1011                Diamond Princess cruise ship                 Others
    ## 1012                                      Fujian         Mainland China
    ## 1013                                   Guangdong         Mainland China
    ## 1014                                     Guangxi         Mainland China
    ## 1015                                     Guizhou         Mainland China
    ## 1016                                      Hainan         Mainland China
    ## 1017                                       Hebei         Mainland China
    ## 1018                                Heilongjiang         Mainland China
    ## 1019                                       Henan         Mainland China
    ## 1020                                   Hong Kong              Hong Kong
    ## 1021                                       Hubei         Mainland China
    ## 1022                                       Hunan         Mainland China
    ## 1023                              Inner Mongolia         Mainland China
    ## 1024                                     Jiangsu         Mainland China
    ## 1025                                     Jiangxi         Mainland China
    ## 1026                                       Jilin         Mainland China
    ## 1027                                    Liaoning         Mainland China
    ## 1028                                     Ningxia         Mainland China
    ## 1029                                     Shaanxi         Mainland China
    ## 1030                                    Shandong         Mainland China
    ## 1031                                    Shanghai         Mainland China
    ## 1032                                     Sichuan         Mainland China
    ## 1033                                     Tianjin         Mainland China
    ## 1034                                    Xinjiang         Mainland China
    ## 1035                                      Yunnan         Mainland China
    ## 1036                                    Zhejiang         Mainland China
    ## 1037                                       Anhui         Mainland China
    ## 1038                                Heilongjiang         Mainland China
    ## 1039                                       Hubei         Mainland China
    ## 1040                                     Jiangxi         Mainland China
    ## 1041                                                              Japan
    ## 1042                                       Anhui         Mainland China
    ## 1043                                     Beijing         Mainland China
    ## 1044                                   Chongqing         Mainland China
    ## 1045                                      Fujian         Mainland China
    ## 1046                                       Gansu         Mainland China
    ## 1047                                   Guangdong         Mainland China
    ## 1048                                     Guangxi         Mainland China
    ## 1049                                     Guizhou         Mainland China
    ## 1050                                       Hebei         Mainland China
    ## 1051                                Heilongjiang         Mainland China
    ## 1052                                       Henan         Mainland China
    ## 1053                                       Hubei         Mainland China
    ## 1054                                       Hunan         Mainland China
    ## 1055                                     Jiangsu         Mainland China
    ## 1056                                     Jiangxi         Mainland China
    ## 1057                                       Jilin         Mainland China
    ## 1058                                    Liaoning         Mainland China
    ## 1059                                     Shaanxi         Mainland China
    ## 1060                                    Shandong         Mainland China
    ## 1061                                    Shanghai         Mainland China
    ## 1062                                     Sichuan         Mainland China
    ## 1063                                     Tianjin         Mainland China
    ## 1064                                      Yunnan         Mainland China
    ## 1065                                    Zhejiang         Mainland China
    ## 1066                                                            Germany
    ## 1067                                                          Singapore
    ## 1068                                                        South Korea
    ## 1069                                                           Thailand
    ## 1070                                                            Vietnam
    ## 1071                                       Anhui         Mainland China
    ## 1072                                     Beijing         Mainland China
    ## 1073                                   Chongqing         Mainland China
    ## 1074                                      Fujian         Mainland China
    ## 1075                                       Gansu         Mainland China
    ## 1076                                   Guangdong         Mainland China
    ## 1077                                     Guangxi         Mainland China
    ## 1078                                     Guizhou         Mainland China
    ## 1079                                      Hainan         Mainland China
    ## 1080                                       Hebei         Mainland China
    ## 1081                                Heilongjiang         Mainland China
    ## 1082                                       Henan         Mainland China
    ## 1083                                   Hong Kong              Hong Kong
    ## 1084                                       Hubei         Mainland China
    ## 1085                                       Hunan         Mainland China
    ## 1086                                     Jiangsu         Mainland China
    ## 1087                                     Jiangxi         Mainland China
    ## 1088                                       Jilin         Mainland China
    ## 1089                                    Liaoning         Mainland China
    ## 1090                                     Ningxia         Mainland China
    ## 1091                        San Diego County, CA                     US
    ## 1092                                     Shaanxi         Mainland China
    ## 1093                                    Shandong         Mainland China
    ## 1094                                    Shanghai         Mainland China
    ## 1095                                      Shanxi         Mainland China
    ## 1096                                     Sichuan         Mainland China
    ## 1097                                     Tianjin         Mainland China
    ## 1098                                    Xinjiang         Mainland China
    ## 1099                                      Yunnan         Mainland China
    ## 1100                                    Zhejiang         Mainland China
    ## 1101                                       Anhui         Mainland China
    ## 1102                                     Beijing         Mainland China
    ## 1103                                   Chongqing         Mainland China
    ## 1104                                Heilongjiang         Mainland China
    ## 1105                                       Henan         Mainland China
    ## 1106                                       Hubei         Mainland China
    ## 1107                                     Tianjin         Mainland China
    ## 1108                                                              Japan
    ## 1109                                                           Malaysia
    ## 1110                                                          Singapore
    ## 1111                                                            Vietnam
    ## 1112                                       Anhui         Mainland China
    ## 1113                                     Beijing         Mainland China
    ## 1114                                   Chongqing         Mainland China
    ## 1115                                      Fujian         Mainland China
    ## 1116                                       Gansu         Mainland China
    ## 1117                                   Guangdong         Mainland China
    ## 1118                                     Guangxi         Mainland China
    ## 1119                                     Guizhou         Mainland China
    ## 1120                                      Hainan         Mainland China
    ## 1121                                       Hebei         Mainland China
    ## 1122                                Heilongjiang         Mainland China
    ## 1123                                       Henan         Mainland China
    ## 1124                                       Hubei         Mainland China
    ## 1125                                       Hunan         Mainland China
    ## 1126                                     Jiangsu         Mainland China
    ## 1127                                     Jiangxi         Mainland China
    ## 1128                                       Jilin         Mainland China
    ## 1129                                    Liaoning         Mainland China
    ## 1130                                     Ningxia         Mainland China
    ## 1131                                     Qinghai         Mainland China
    ## 1132                                     Shaanxi         Mainland China
    ## 1133                                    Shandong         Mainland China
    ## 1134                                    Shanghai         Mainland China
    ## 1135                                      Shanxi         Mainland China
    ## 1136                                     Sichuan         Mainland China
    ## 1137                                     Tianjin         Mainland China
    ## 1138                                    Xinjiang         Mainland China
    ## 1139                                      Yunnan         Mainland China
    ## 1140                                    Zhejiang         Mainland China
    ## 1141                                                              Japan
    ## 1142                                                          Singapore
    ## 1143                                                                 UK
    ## 1144                                       Anhui         Mainland China
    ## 1145                                     Beijing         Mainland China
    ## 1146                                   Chongqing         Mainland China
    ## 1147                Diamond Princess cruise ship                 Others
    ## 1148                                      Fujian         Mainland China
    ## 1149                                       Gansu         Mainland China
    ## 1150                                   Guangdong         Mainland China
    ## 1151                                     Guangxi         Mainland China
    ## 1152                                     Guizhou         Mainland China
    ## 1153                                      Hainan         Mainland China
    ## 1154                                       Hebei         Mainland China
    ## 1155                                Heilongjiang         Mainland China
    ## 1156                                       Henan         Mainland China
    ## 1157                                   Hong Kong              Hong Kong
    ## 1158                                       Hunan         Mainland China
    ## 1159                              Inner Mongolia         Mainland China
    ## 1160                                     Jiangsu         Mainland China
    ## 1161                                     Jiangxi         Mainland China
    ## 1162                                       Jilin         Mainland China
    ## 1163                                    Liaoning         Mainland China
    ## 1164                                     Ningxia         Mainland China
    ## 1165                                     Shaanxi         Mainland China
    ## 1166                                    Shandong         Mainland China
    ## 1167                                    Shanghai         Mainland China
    ## 1168                                      Shanxi         Mainland China
    ## 1169                                     Sichuan         Mainland China
    ## 1170                                     Tianjin         Mainland China
    ## 1171                                    Xinjiang         Mainland China
    ## 1172                                      Yunnan         Mainland China
    ## 1173                                    Zhejiang         Mainland China
    ## 1174                                      Hainan         Mainland China
    ## 1175                                       Henan         Mainland China
    ## 1176                                       Hunan         Mainland China
    ## 1177                                    Liaoning         Mainland China
    ## 1178                                    Shandong         Mainland China
    ## 1179                                                           Cambodia
    ## 1180                                                            Finland
    ## 1181                                                             France
    ## 1182                                                              Nepal
    ## 1183                                                        Philippines
    ## 1184                                                             Russia
    ## 1185                                                          Singapore
    ## 1186                                                        South Korea
    ## 1187                                                                 UK
    ## 1188                                               United Arab Emirates
    ## 1189                                       Anhui         Mainland China
    ## 1190                                     Beijing         Mainland China
    ## 1191                                   Chongqing         Mainland China
    ## 1192                                      Fujian         Mainland China
    ## 1193                                       Gansu         Mainland China
    ## 1194                                   Guangdong         Mainland China
    ## 1195                                     Guangxi         Mainland China
    ## 1196                                     Guizhou         Mainland China
    ## 1197                                      Hainan         Mainland China
    ## 1198                                       Hebei         Mainland China
    ## 1199                                Heilongjiang         Mainland China
    ## 1200                                       Henan         Mainland China
    ## 1201                                   Hong Kong              Hong Kong
    ## 1202                                       Hubei         Mainland China
    ## 1203                                       Hunan         Mainland China
    ## 1204                              Inner Mongolia         Mainland China
    ## 1205                                     Jiangsu         Mainland China
    ## 1206                                     Jiangxi         Mainland China
    ## 1207                                       Jilin         Mainland China
    ## 1208                                    Liaoning         Mainland China
    ## 1209                                  London, ON                 Canada
    ## 1210                                       Macau                  Macau
    ## 1211                                     Ningxia         Mainland China
    ## 1212                                     Qinghai         Mainland China
    ## 1213                                     Shaanxi         Mainland China
    ## 1214                                    Shandong         Mainland China
    ## 1215                                    Shanghai         Mainland China
    ## 1216                                      Shanxi         Mainland China
    ## 1217                                     Sichuan         Mainland China
    ## 1218                                     Tianjin         Mainland China
    ## 1219                                       Tibet         Mainland China
    ## 1220                                      Yunnan         Mainland China
    ## 1221                                    Zhejiang         Mainland China
    ## 1222                                                           Malaysia
    ## 1223                                                          Singapore
    ## 1224                                                            Vietnam
    ## 1225                                       Anhui         Mainland China
    ## 1226                                     Beijing         Mainland China
    ## 1227                                   Chongqing         Mainland China
    ## 1228                                      Fujian         Mainland China
    ## 1229                                       Gansu         Mainland China
    ## 1230                                   Guangdong         Mainland China
    ## 1231                                     Guizhou         Mainland China
    ## 1232                                       Hebei         Mainland China
    ## 1233                                Heilongjiang         Mainland China
    ## 1234                                       Henan         Mainland China
    ## 1235                                   Hong Kong              Hong Kong
    ## 1236                                       Hubei         Mainland China
    ## 1237                                       Hunan         Mainland China
    ## 1238                              Inner Mongolia         Mainland China
    ## 1239                                     Jiangsu         Mainland China
    ## 1240                                     Jiangxi         Mainland China
    ## 1241                                       Jilin         Mainland China
    ## 1242                                    Liaoning         Mainland China
    ## 1243                                     Ningxia         Mainland China
    ## 1244                             San Antonio, TX                     US
    ## 1245                        San Diego County, CA                     US
    ## 1246                                     Shaanxi         Mainland China
    ## 1247                                    Shandong         Mainland China
    ## 1248                                    Shanghai         Mainland China
    ## 1249                                     Sichuan         Mainland China
    ## 1250                                     Tianjin         Mainland China
    ## 1251                                    Xinjiang         Mainland China
    ## 1252                                      Yunnan         Mainland China
    ## 1253                                    Zhejiang         Mainland China
    ## 1254                                                              Japan
    ## 1255                                       Anhui         Mainland China
    ## 1256                                   Chongqing         Mainland China
    ## 1257                                   Guangdong         Mainland China
    ## 1258                                     Guangxi         Mainland China
    ## 1259                                       Hebei         Mainland China
    ## 1260                                Heilongjiang         Mainland China
    ## 1261                                       Henan         Mainland China
    ## 1262                                       Hubei         Mainland China
    ## 1263                                     Tianjin         Mainland China
    ## 1264                                    Xinjiang         Mainland China
    ## 1265                                                            Germany
    ## 1266                                                           Thailand
    ## 1267                                                            Vietnam
    ## 1268                                       Anhui         Mainland China
    ## 1269                                     Beijing         Mainland China
    ## 1270                                   Chongqing         Mainland China
    ## 1271                                      Fujian         Mainland China
    ## 1272                                       Gansu         Mainland China
    ## 1273                                   Guangdong         Mainland China
    ## 1274                                     Guangxi         Mainland China
    ## 1275                                     Guizhou         Mainland China
    ## 1276                                      Hainan         Mainland China
    ## 1277                                       Hebei         Mainland China
    ## 1278                                Heilongjiang         Mainland China
    ## 1279                                       Henan         Mainland China
    ## 1280                                       Hubei         Mainland China
    ## 1281                                       Hunan         Mainland China
    ## 1282                                     Jiangsu         Mainland China
    ## 1283                                     Jiangxi         Mainland China
    ## 1284                                       Jilin         Mainland China
    ## 1285                                    Liaoning         Mainland China
    ## 1286                                       Macau                  Macau
    ## 1287                             New South Wales              Australia
    ## 1288                                     Qinghai         Mainland China
    ## 1289                                     Shaanxi         Mainland China
    ## 1290                                    Shandong         Mainland China
    ## 1291                                    Shanghai         Mainland China
    ## 1292                                      Shanxi         Mainland China
    ## 1293                                     Sichuan         Mainland China
    ## 1294                                     Tianjin         Mainland China
    ## 1295                                    Victoria              Australia
    ## 1296                                    Xinjiang         Mainland China
    ## 1297                                      Yunnan         Mainland China
    ## 1298                                    Zhejiang         Mainland China
    ## 1299                                                              Egypt
    ## 1300                                                              Japan
    ## 1301                                                          Singapore
    ## 1302                                       Anhui         Mainland China
    ## 1303                                     Beijing         Mainland China
    ## 1304                                   Chongqing         Mainland China
    ## 1305                Diamond Princess cruise ship                 Others
    ## 1306                                      Fujian         Mainland China
    ## 1307                                   Guangdong         Mainland China
    ## 1308                                     Guangxi         Mainland China
    ## 1309                                     Guizhou         Mainland China
    ## 1310                                      Hainan         Mainland China
    ## 1311                                       Hebei         Mainland China
    ## 1312                                Heilongjiang         Mainland China
    ## 1313                                       Henan         Mainland China
    ## 1314                                   Hong Kong              Hong Kong
    ## 1315                                       Hubei         Mainland China
    ## 1316                                       Hunan         Mainland China
    ## 1317                              Inner Mongolia         Mainland China
    ## 1318                                     Jiangsu         Mainland China
    ## 1319                                     Jiangxi         Mainland China
    ## 1320                                       Jilin         Mainland China
    ## 1321                                    Liaoning         Mainland China
    ## 1322                                     Ningxia         Mainland China
    ## 1323                                     Shaanxi         Mainland China
    ## 1324                                    Shandong         Mainland China
    ## 1325                                    Shanghai         Mainland China
    ## 1326                                      Shanxi         Mainland China
    ## 1327                                     Sichuan         Mainland China
    ## 1328                                     Tianjin         Mainland China
    ## 1329                                    Xinjiang         Mainland China
    ## 1330                                      Yunnan         Mainland China
    ## 1331                                    Zhejiang         Mainland China
    ## 1332                                       Anhui         Mainland China
    ## 1333                                   Chongqing         Mainland China
    ## 1334                                Heilongjiang         Mainland China
    ## 1335                                       Henan         Mainland China
    ## 1336                                       Hubei         Mainland China
    ## 1337                                                          Singapore
    ## 1338                                       Anhui         Mainland China
    ## 1339                                     Beijing         Mainland China
    ## 1340                                   Chongqing         Mainland China
    ## 1341                                      Fujian         Mainland China
    ## 1342                                   Guangdong         Mainland China
    ## 1343                                     Guangxi         Mainland China
    ## 1344                                     Guizhou         Mainland China
    ## 1345                                      Hainan         Mainland China
    ## 1346                                       Hebei         Mainland China
    ## 1347                                Heilongjiang         Mainland China
    ## 1348                                       Henan         Mainland China
    ## 1349                                       Hubei         Mainland China
    ## 1350                                       Hunan         Mainland China
    ## 1351                                     Jiangsu         Mainland China
    ## 1352                                     Jiangxi         Mainland China
    ## 1353                                       Jilin         Mainland China
    ## 1354                                    Liaoning         Mainland China
    ## 1355                                     Shaanxi         Mainland China
    ## 1356                                    Shandong         Mainland China
    ## 1357                                    Shanghai         Mainland China
    ## 1358                                      Shanxi         Mainland China
    ## 1359                                     Sichuan         Mainland China
    ## 1360                                      Taiwan                 Taiwan
    ## 1361                                     Tianjin         Mainland China
    ## 1362                                      Yunnan         Mainland China
    ## 1363                                    Zhejiang         Mainland China
    ## 1364                                                             France
    ## 1365                                                              Japan
    ## 1366                                                           Malaysia
    ## 1367                                                          Singapore
    ## 1368                                       Anhui         Mainland China
    ## 1369                                     Beijing         Mainland China
    ## 1370                                   Chongqing         Mainland China
    ## 1371                Diamond Princess cruise ship                 Others
    ## 1372                                      Fujian         Mainland China
    ## 1373                                   Guangdong         Mainland China
    ## 1374                                     Guangxi         Mainland China
    ## 1375                                     Guizhou         Mainland China
    ## 1376                                      Hainan         Mainland China
    ## 1377                                       Hebei         Mainland China
    ## 1378                                Heilongjiang         Mainland China
    ## 1379                                       Henan         Mainland China
    ## 1380                                       Hubei         Mainland China
    ## 1381                                       Hunan         Mainland China
    ## 1382                              Inner Mongolia         Mainland China
    ## 1383                                     Jiangsu         Mainland China
    ## 1384                                     Jiangxi         Mainland China
    ## 1385                                       Jilin         Mainland China
    ## 1386                                     Ningxia         Mainland China
    ## 1387                                     Shaanxi         Mainland China
    ## 1388                                    Shandong         Mainland China
    ## 1389                                    Shanghai         Mainland China
    ## 1390                                      Shanxi         Mainland China
    ## 1391                                     Sichuan         Mainland China
    ## 1392                                     Tianjin         Mainland China
    ## 1393                                    Xinjiang         Mainland China
    ## 1394                                      Yunnan         Mainland China
    ## 1395                                    Zhejiang         Mainland China
    ## 1396                                                             France
    ## 1397                                     Beijing         Mainland China
    ## 1398                                       Henan         Mainland China
    ## 1399                                       Hubei         Mainland China
    ## 1400                                                             France
    ## 1401                                                              Japan
    ## 1402                                                           Malaysia
    ## 1403                                                          Singapore
    ## 1404                                                        South Korea
    ## 1405                                                              Spain
    ## 1406                                               United Arab Emirates
    ## 1407                                       Anhui         Mainland China
    ## 1408                                     Beijing         Mainland China
    ## 1409                                   Chongqing         Mainland China
    ## 1410                                      Fujian         Mainland China
    ## 1411                                       Gansu         Mainland China
    ## 1412                                   Guangdong         Mainland China
    ## 1413                                     Guangxi         Mainland China
    ## 1414                                     Guizhou         Mainland China
    ## 1415                                      Hainan         Mainland China
    ## 1416                                       Hebei         Mainland China
    ## 1417                                Heilongjiang         Mainland China
    ## 1418                                       Henan         Mainland China
    ## 1419                                       Hubei         Mainland China
    ## 1420                                       Hunan         Mainland China
    ## 1421                              Inner Mongolia         Mainland China
    ## 1422                                     Jiangsu         Mainland China
    ## 1423                                     Jiangxi         Mainland China
    ## 1424                                       Jilin         Mainland China
    ## 1425                                    Liaoning         Mainland China
    ## 1426                                     Ningxia         Mainland China
    ## 1427                                     Qinghai         Mainland China
    ## 1428                                     Shaanxi         Mainland China
    ## 1429                                    Shandong         Mainland China
    ## 1430                                    Shanghai         Mainland China
    ## 1431                                      Shanxi         Mainland China
    ## 1432                                     Sichuan         Mainland China
    ## 1433                                     Tianjin         Mainland China
    ## 1434                                    Xinjiang         Mainland China
    ## 1435                                      Yunnan         Mainland China
    ## 1436                                    Zhejiang         Mainland China
    ## 1437                                                              Japan
    ## 1438                                                          Singapore
    ## 1439                                                        South Korea
    ## 1440                                                           Thailand
    ## 1441                                               United Arab Emirates
    ## 1442                                       Anhui         Mainland China
    ## 1443                                     Beijing         Mainland China
    ## 1444                                   Chongqing         Mainland China
    ## 1445                Diamond Princess cruise ship                 Others
    ## 1446                                      Fujian         Mainland China
    ## 1447                                   Guangdong         Mainland China
    ## 1448                                     Guangxi         Mainland China
    ## 1449                                     Guizhou         Mainland China
    ## 1450                                       Hebei         Mainland China
    ## 1451                                Heilongjiang         Mainland China
    ## 1452                                       Henan         Mainland China
    ## 1453                                   Hong Kong              Hong Kong
    ## 1454                                       Hubei         Mainland China
    ## 1455                                       Hunan         Mainland China
    ## 1456                              Inner Mongolia         Mainland China
    ## 1457                                     Jiangsu         Mainland China
    ## 1458                                     Jiangxi         Mainland China
    ## 1459                                       Jilin         Mainland China
    ## 1460                                    Liaoning         Mainland China
    ## 1461                                     Shaanxi         Mainland China
    ## 1462                                    Shandong         Mainland China
    ## 1463                                    Shanghai         Mainland China
    ## 1464                                      Shanxi         Mainland China
    ## 1465                                     Sichuan         Mainland China
    ## 1466                                      Taiwan                 Taiwan
    ## 1467                                     Tianjin         Mainland China
    ## 1468                                    Xinjiang         Mainland China
    ## 1469                                      Yunnan         Mainland China
    ## 1470                                    Zhejiang         Mainland China
    ## 1471                                       Hubei         Mainland China
    ## 1472                                       Hunan         Mainland China
    ## 1473                                     Sichuan         Mainland China
    ## 1474                                      Taiwan                 Taiwan
    ## 1475                                                              India
    ## 1476                                                           Thailand
    ## 1477                                                                 UK
    ## 1478                                               United Arab Emirates
    ## 1479                                       Anhui         Mainland China
    ## 1480                                     Beijing         Mainland China
    ## 1481                                   Chongqing         Mainland China
    ## 1482                                      Fujian         Mainland China
    ## 1483                                       Gansu         Mainland China
    ## 1484                                   Guangdong         Mainland China
    ## 1485                                     Guangxi         Mainland China
    ## 1486                                     Guizhou         Mainland China
    ## 1487                                      Hainan         Mainland China
    ## 1488                                       Hebei         Mainland China
    ## 1489                                Heilongjiang         Mainland China
    ## 1490                                       Henan         Mainland China
    ## 1491                                   Hong Kong              Hong Kong
    ## 1492                                       Hubei         Mainland China
    ## 1493                                       Hunan         Mainland China
    ## 1494                              Inner Mongolia         Mainland China
    ## 1495                                     Jiangsu         Mainland China
    ## 1496                                     Jiangxi         Mainland China
    ## 1497                                       Jilin         Mainland China
    ## 1498                                    Liaoning         Mainland China
    ## 1499                                       Macau                  Macau
    ## 1500                                     Shaanxi         Mainland China
    ## 1501                                    Shandong         Mainland China
    ## 1502                                    Shanghai         Mainland China
    ## 1503                                      Shanxi         Mainland China
    ## 1504                                     Sichuan         Mainland China
    ## 1505                                     Tianjin         Mainland China
    ## 1506                                    Xinjiang         Mainland China
    ## 1507                                    Zhejiang         Mainland China
    ## 1508                                                              Japan
    ## 1509                                                          Singapore
    ## 1510                                                        South Korea
    ## 1511                                                           Thailand
    ## 1512                                       Anhui         Mainland China
    ## 1513                                     Beijing         Mainland China
    ## 1514                            British Columbia                 Canada
    ## 1515                                   Chongqing         Mainland China
    ## 1516                Diamond Princess cruise ship                 Others
    ## 1517                                      Fujian         Mainland China
    ## 1518                                       Gansu         Mainland China
    ## 1519                                   Guangdong         Mainland China
    ## 1520                                     Guangxi         Mainland China
    ## 1521                                     Guizhou         Mainland China
    ## 1522                                      Hainan         Mainland China
    ## 1523                                       Hebei         Mainland China
    ## 1524                                Heilongjiang         Mainland China
    ## 1525                                       Henan         Mainland China
    ## 1526                                   Hong Kong              Hong Kong
    ## 1527                                       Hubei         Mainland China
    ## 1528                                       Hunan         Mainland China
    ## 1529                              Inner Mongolia         Mainland China
    ## 1530                                     Jiangsu         Mainland China
    ## 1531                                     Jiangxi         Mainland China
    ## 1532                                     Shaanxi         Mainland China
    ## 1533                                    Shandong         Mainland China
    ## 1534                                    Shanghai         Mainland China
    ## 1535                                      Shanxi         Mainland China
    ## 1536                                     Sichuan         Mainland China
    ## 1537                                      Taiwan                 Taiwan
    ## 1538                                     Tianjin         Mainland China
    ## 1539                                    Xinjiang         Mainland China
    ## 1540                                    Zhejiang         Mainland China
    ## 1541                                   Guangdong         Mainland China
    ## 1542                                       Henan         Mainland China
    ## 1543                                       Hubei         Mainland China
    ## 1544                                                            Belgium
    ## 1545                                                          Singapore
    ## 1546                                                        South Korea
    ## 1547                                                           Thailand
    ## 1548                                       Anhui         Mainland China
    ## 1549                                     Beijing         Mainland China
    ## 1550                                   Chongqing         Mainland China
    ## 1551                                      Fujian         Mainland China
    ## 1552                                       Gansu         Mainland China
    ## 1553                                   Guangdong         Mainland China
    ## 1554                                     Guangxi         Mainland China
    ## 1555                                     Guizhou         Mainland China
    ## 1556                                      Hainan         Mainland China
    ## 1557                                       Hebei         Mainland China
    ## 1558                                Heilongjiang         Mainland China
    ## 1559                                       Henan         Mainland China
    ## 1560                                       Hubei         Mainland China
    ## 1561                                       Hunan         Mainland China
    ## 1562                                     Jiangsu         Mainland China
    ## 1563                                     Jiangxi         Mainland China
    ## 1564                                       Jilin         Mainland China
    ## 1565                                    Liaoning         Mainland China
    ## 1566                                     Ningxia         Mainland China
    ## 1567                                     Shaanxi         Mainland China
    ## 1568                                    Shandong         Mainland China
    ## 1569                                    Shanghai         Mainland China
    ## 1570                                      Shanxi         Mainland China
    ## 1571                                     Sichuan         Mainland China
    ## 1572                             South Australia              Australia
    ## 1573                                     Tianjin         Mainland China
    ## 1574                                      Yunnan         Mainland China
    ## 1575                                    Zhejiang         Mainland China
    ## 1576                                                              Japan
    ## 1577                                                          Singapore
    ## 1578                                                        South Korea
    ## 1579                                       Anhui         Mainland China
    ## 1580                                     Beijing         Mainland China
    ## 1581                                   Chongqing         Mainland China
    ## 1582                Diamond Princess cruise ship                 Others
    ## 1583                                      Fujian         Mainland China
    ## 1584                                   Guangdong         Mainland China
    ## 1585                                     Guangxi         Mainland China
    ## 1586                                       Hebei         Mainland China
    ## 1587                                Heilongjiang         Mainland China
    ## 1588                                       Henan         Mainland China
    ## 1589                                   Hong Kong              Hong Kong
    ## 1590                                       Hubei         Mainland China
    ## 1591                                       Hunan         Mainland China
    ## 1592                              Inner Mongolia         Mainland China
    ## 1593                                     Jiangsu         Mainland China
    ## 1594                                     Jiangxi         Mainland China
    ## 1595                                    Shandong         Mainland China
    ## 1596                                      Shanxi         Mainland China
    ## 1597                                     Sichuan         Mainland China
    ## 1598                                     Tianjin         Mainland China
    ## 1599                                    Xinjiang         Mainland China
    ## 1600                                      Yunnan         Mainland China
    ## 1601                                    Zhejiang         Mainland China
    ## 1602                                     Guizhou         Mainland China
    ## 1603                                       Hebei         Mainland China
    ## 1604                                       Henan         Mainland China
    ## 1605                                       Hubei         Mainland China
    ## 1606                                       Hunan         Mainland China
    ## 1607                                    Shandong         Mainland China
    ## 1608                                                            Germany
    ## 1609                                                              Japan
    ## 1610                                                           Malaysia
    ## 1611                                                          Singapore
    ## 1612                                                        South Korea
    ## 1613                                       Anhui         Mainland China
    ## 1614                                     Beijing         Mainland China
    ## 1615                                   Chongqing         Mainland China
    ## 1616                                      Fujian         Mainland China
    ## 1617                                       Gansu         Mainland China
    ## 1618                                   Guangdong         Mainland China
    ## 1619                                     Guangxi         Mainland China
    ## 1620                                     Guizhou         Mainland China
    ## 1621                                      Hainan         Mainland China
    ## 1622                                       Hebei         Mainland China
    ## 1623                                Heilongjiang         Mainland China
    ## 1624                                       Henan         Mainland China
    ## 1625                                       Hubei         Mainland China
    ## 1626                                       Hunan         Mainland China
    ## 1627                                     Jiangsu         Mainland China
    ## 1628                                     Jiangxi         Mainland China
    ## 1629                                       Jilin         Mainland China
    ## 1630                                    Liaoning         Mainland China
    ## 1631                                     Ningxia         Mainland China
    ## 1632                                     Qinghai         Mainland China
    ## 1633                                     Shaanxi         Mainland China
    ## 1634                                    Shandong         Mainland China
    ## 1635                                    Shanghai         Mainland China
    ## 1636                                      Shanxi         Mainland China
    ## 1637                                     Sichuan         Mainland China
    ## 1638                                     Tianjin         Mainland China
    ## 1639                                      Yunnan         Mainland China
    ## 1640                                    Zhejiang         Mainland China
    ## 1641                                                               Iran
    ## 1642                                                              Japan
    ## 1643                                                          Singapore
    ## 1644                                       Anhui         Mainland China
    ## 1645                                     Beijing         Mainland China
    ## 1646                                   Chongqing         Mainland China
    ## 1647                Diamond Princess cruise ship                 Others
    ## 1648                                      Fujian         Mainland China
    ## 1649                                   Guangdong         Mainland China
    ## 1650                                     Guangxi         Mainland China
    ## 1651                                      Hainan         Mainland China
    ## 1652                                Heilongjiang         Mainland China
    ## 1653                                       Henan         Mainland China
    ## 1654                                   Hong Kong              Hong Kong
    ## 1655                                       Hubei         Mainland China
    ## 1656                                       Hunan         Mainland China
    ## 1657                              Inner Mongolia         Mainland China
    ## 1658                                     Jiangsu         Mainland China
    ## 1659                                     Jiangxi         Mainland China
    ## 1660                                       Jilin         Mainland China
    ## 1661                                     Ningxia         Mainland China
    ## 1662                                     Shaanxi         Mainland China
    ## 1663                                    Shandong         Mainland China
    ## 1664                                     Sichuan         Mainland China
    ## 1665                                      Taiwan                 Taiwan
    ## 1666                                     Tianjin         Mainland China
    ## 1667                                    Zhejiang         Mainland China
    ## 1668                                                               Iran
    ## 1669                                   Guangdong         Mainland China
    ## 1670                                Heilongjiang         Mainland China
    ## 1671                                   Hong Kong              Hong Kong
    ## 1672                                       Hubei         Mainland China
    ## 1673                                    Shanghai         Mainland China
    ## 1674                                      Yunnan         Mainland China
    ## 1675                                                              Japan
    ## 1676                                                           Malaysia
    ## 1677                                                          Singapore
    ## 1678                                       Anhui         Mainland China
    ## 1679                                     Beijing         Mainland China
    ## 1680                                   Chongqing         Mainland China
    ## 1681                Diamond Princess cruise ship                 Others
    ## 1682                                      Fujian         Mainland China
    ## 1683                                       Gansu         Mainland China
    ## 1684                                   Guangdong         Mainland China
    ## 1685                                     Guangxi         Mainland China
    ## 1686                                     Guizhou         Mainland China
    ## 1687                                      Hainan         Mainland China
    ## 1688                                       Hebei         Mainland China
    ## 1689                                Heilongjiang         Mainland China
    ## 1690                                       Henan         Mainland China
    ## 1691                                   Hong Kong              Hong Kong
    ## 1692                                       Hubei         Mainland China
    ## 1693                                       Hunan         Mainland China
    ## 1694                              Inner Mongolia         Mainland China
    ## 1695                                     Jiangsu         Mainland China
    ## 1696                                     Jiangxi         Mainland China
    ## 1697                                       Jilin         Mainland China
    ## 1698                                    Liaoning         Mainland China
    ## 1699                                     Qinghai         Mainland China
    ## 1700                                     Shaanxi         Mainland China
    ## 1701                                    Shandong         Mainland China
    ## 1702                                    Shanghai         Mainland China
    ## 1703                                      Shanxi         Mainland China
    ## 1704                                     Sichuan         Mainland China
    ## 1705                                     Tianjin         Mainland China
    ## 1706                                    Xinjiang         Mainland China
    ## 1707                                      Yunnan         Mainland China
    ## 1708                                    Zhejiang         Mainland China
    ## 1709                                                               Iran
    ## 1710                                                              Japan
    ## 1711                                                        South Korea
    ## 1712                                       Anhui         Mainland China
    ## 1713                                     Beijing         Mainland China
    ## 1714                                   Chongqing         Mainland China
    ## 1715                Diamond Princess cruise ship                 Others
    ## 1716                                   Guangdong         Mainland China
    ## 1717                                     Guangxi         Mainland China
    ## 1718                                       Hebei         Mainland China
    ## 1719                                Heilongjiang         Mainland China
    ## 1720                                       Henan         Mainland China
    ## 1721                                   Hong Kong              Hong Kong
    ## 1722                                       Hubei         Mainland China
    ## 1723                                       Hunan         Mainland China
    ## 1724                                       Jilin         Mainland China
    ## 1725                                     Shaanxi         Mainland China
    ## 1726                                    Shandong         Mainland China
    ## 1727                                    Shanghai         Mainland China
    ## 1728                                      Shanxi         Mainland China
    ## 1729                                     Sichuan         Mainland China
    ## 1730                                      Taiwan                 Taiwan
    ## 1731                                     Tianjin         Mainland China
    ## 1732                                      Yunnan         Mainland China
    ## 1733                                    Zhejiang         Mainland China
    ## 1734                                                        South Korea
    ## 1735                                   Chongqing         Mainland China
    ## 1736                Diamond Princess cruise ship                 Others
    ## 1737                                      Fujian         Mainland China
    ## 1738                                       Hebei         Mainland China
    ## 1739                                       Hubei         Mainland China
    ## 1740                                     Shaanxi         Mainland China
    ## 1741                                    Shandong         Mainland China
    ## 1742                                      Yunnan         Mainland China
    ## 1743                                    Zhejiang         Mainland China
    ## 1744                                                        South Korea
    ## 1745                                       Anhui         Mainland China
    ## 1746                                     Beijing         Mainland China
    ## 1747                                   Chongqing         Mainland China
    ## 1748                                      Fujian         Mainland China
    ## 1749                                       Gansu         Mainland China
    ## 1750                                   Guangdong         Mainland China
    ## 1751                                     Guangxi         Mainland China
    ## 1752                                     Guizhou         Mainland China
    ## 1753                                      Hainan         Mainland China
    ## 1754                                       Hebei         Mainland China
    ## 1755                                Heilongjiang         Mainland China
    ## 1756                                       Henan         Mainland China
    ## 1757                                   Hong Kong              Hong Kong
    ## 1758                                       Hubei         Mainland China
    ## 1759                                       Hunan         Mainland China
    ## 1760                              Inner Mongolia         Mainland China
    ## 1761                                     Jiangsu         Mainland China
    ## 1762                                     Jiangxi         Mainland China
    ## 1763                                       Jilin         Mainland China
    ## 1764                                    Liaoning         Mainland China
    ## 1765                                       Macau                  Macau
    ## 1766                                     Ningxia         Mainland China
    ## 1767                                     Shaanxi         Mainland China
    ## 1768                                    Shandong         Mainland China
    ## 1769                                    Shanghai         Mainland China
    ## 1770                                      Shanxi         Mainland China
    ## 1771                                     Sichuan         Mainland China
    ## 1772                                     Tianjin         Mainland China
    ## 1773                                    Xinjiang         Mainland China
    ## 1774                                      Yunnan         Mainland China
    ## 1775                                    Zhejiang         Mainland China
    ## 1776                                                               Iran
    ## 1777                                                             Israel
    ## 1778                                                              Italy
    ## 1779                                                              Japan
    ## 1780                                                            Lebanon
    ## 1781                                                          Singapore
    ## 1782                                                        South Korea
    ## 1783                                       Anhui         Mainland China
    ## 1784                                     Beijing         Mainland China
    ## 1785                            British Columbia                 Canada
    ## 1786                                   Chongqing         Mainland China
    ## 1787                       From Diamond Princess              Australia
    ## 1788                                   Guangdong         Mainland China
    ## 1789                                     Guangxi         Mainland China
    ## 1790                                       Hebei         Mainland China
    ## 1791                                Heilongjiang         Mainland China
    ## 1792                                       Henan         Mainland China
    ## 1793                                       Hubei         Mainland China
    ## 1794                         Humboldt County, CA                     US
    ## 1795                                       Hunan         Mainland China
    ## 1796        Lackland, TX (From Diamond Princess)                     US
    ## 1797           Omaha, NE (From Diamond Princess)                     US
    ## 1798                       Sacramento County, CA                     US
    ## 1799                                    Shandong         Mainland China
    ## 1800                                     Sichuan         Mainland China
    ## 1801                                      Taiwan                 Taiwan
    ## 1802                                     Tianjin         Mainland China
    ## 1803          Travis, CA (From Diamond Princess)                     US
    ## 1804                                    Zhejiang         Mainland China
    ## 1805                                                               Iran
    ## 1806                                                              Italy
    ## 1807                                                        South Korea
    ## 1808                                                            Germany
    ## 1809                                                              Japan
    ## 1810                                                          Singapore
    ## 1811                                                           Thailand
    ## 1812                                                            Vietnam
    ## 1813                                       Anhui         Mainland China
    ## 1814                                     Beijing         Mainland China
    ## 1815                                   Chongqing         Mainland China
    ## 1816                                      Fujian         Mainland China
    ## 1817                                       Gansu         Mainland China
    ## 1818                                   Guangdong         Mainland China
    ## 1819                                     Guangxi         Mainland China
    ## 1820                                     Guizhou         Mainland China
    ## 1821                                      Hainan         Mainland China
    ## 1822                                       Hebei         Mainland China
    ## 1823                                Heilongjiang         Mainland China
    ## 1824                                       Henan         Mainland China
    ## 1825                                   Hong Kong              Hong Kong
    ## 1826                                       Hubei         Mainland China
    ## 1827                                       Hunan         Mainland China
    ## 1828                              Inner Mongolia         Mainland China
    ## 1829                                     Jiangsu         Mainland China
    ## 1830                                     Jiangxi         Mainland China
    ## 1831                                       Jilin         Mainland China
    ## 1832                                    Liaoning         Mainland China
    ## 1833                                     Ningxia         Mainland China
    ## 1834                                     Qinghai         Mainland China
    ## 1835                                  Queensland              Australia
    ## 1836                        San Diego County, CA                     US
    ## 1837                             Santa Clara, CA                     US
    ## 1838                                     Shaanxi         Mainland China
    ## 1839                                    Shandong         Mainland China
    ## 1840                                    Shanghai         Mainland China
    ## 1841                                      Shanxi         Mainland China
    ## 1842                                     Sichuan         Mainland China
    ## 1843                                     Tianjin         Mainland China
    ## 1844                                 Toronto, ON                 Canada
    ## 1845                                    Xinjiang         Mainland China
    ## 1846                                      Yunnan         Mainland China
    ## 1847                                    Zhejiang         Mainland China
    ## 1848                                                               Iran
    ## 1849                                                              Italy
    ## 1850                                                              Japan
    ## 1851                                                        South Korea
    ## 1852                                               United Arab Emirates
    ## 1853                                       Anhui         Mainland China
    ## 1854                                     Beijing         Mainland China
    ## 1855                                   Chongqing         Mainland China
    ## 1856                       From Diamond Princess              Australia
    ## 1857                                   Guangdong         Mainland China
    ## 1858                                     Guangxi         Mainland China
    ## 1859                                       Hebei         Mainland China
    ## 1860                                       Henan         Mainland China
    ## 1861                                   Hong Kong              Hong Kong
    ## 1862                                       Hubei         Mainland China
    ## 1863                                       Hunan         Mainland China
    ## 1864                                    Shandong         Mainland China
    ## 1865                                    Shanghai         Mainland China
    ## 1866                                     Sichuan         Mainland China
    ## 1867                                     Tianjin         Mainland China
    ## 1868                                    Zhejiang         Mainland China
    ## 1869                                                               Iran
    ## 1870                                                              Italy
    ## 1871                                       Hebei         Mainland China
    ## 1872                                       Hubei         Mainland China
    ## 1873                                    Shanghai         Mainland China
    ## 1874                                    Xinjiang         Mainland China
    ## 1875                                                              Italy
    ## 1876                                       Anhui         Mainland China
    ## 1877                                     Beijing         Mainland China
    ## 1878                                   Chongqing         Mainland China
    ## 1879                                      Fujian         Mainland China
    ## 1880                                   Guangdong         Mainland China
    ## 1881                                     Guangxi         Mainland China
    ## 1882                                     Guizhou         Mainland China
    ## 1883                                      Hainan         Mainland China
    ## 1884                                       Hebei         Mainland China
    ## 1885                                Heilongjiang         Mainland China
    ## 1886                                       Henan         Mainland China
    ## 1887                                   Hong Kong              Hong Kong
    ## 1888                                       Hubei         Mainland China
    ## 1889                                       Hunan         Mainland China
    ## 1890                              Inner Mongolia         Mainland China
    ## 1891                                     Jiangsu         Mainland China
    ## 1892                                     Jiangxi         Mainland China
    ## 1893                                       Jilin         Mainland China
    ## 1894                                    Liaoning         Mainland China
    ## 1895                                     Shaanxi         Mainland China
    ## 1896                                    Shandong         Mainland China
    ## 1897                                    Shanghai         Mainland China
    ## 1898                                      Shanxi         Mainland China
    ## 1899                                     Sichuan         Mainland China
    ## 1900                                     Tianjin         Mainland China
    ## 1901                                    Xinjiang         Mainland China
    ## 1902                                      Yunnan         Mainland China
    ## 1903                                    Zhejiang         Mainland China
    ## 1904                                                               Iran
    ## 1905                                                              Italy
    ## 1906                                                              Japan
    ## 1907                                                          Singapore
    ## 1908                                                        South Korea
    ## 1909                                   Chongqing         Mainland China
    ## 1910                Diamond Princess cruise ship                 Others
    ## 1911                                   Guangdong         Mainland China
    ## 1912                                       Hebei         Mainland China
    ## 1913                                Heilongjiang         Mainland China
    ## 1914                                       Henan         Mainland China
    ## 1915                                   Hong Kong              Hong Kong
    ## 1916                                       Hunan         Mainland China
    ## 1917                                    Shandong         Mainland China
    ## 1918                                      Taiwan                 Taiwan
    ## 1919                                                               Iran
    ## 1920                                                              Italy
    ## 1921                                                        South Korea
    ## 1922                Diamond Princess cruise ship                 Others
    ## 1923                                   Guangdong         Mainland China
    ## 1924                                      Hainan         Mainland China
    ## 1925                                                              Italy
    ## 1926                                                          Singapore
    ## 1927                                                        South Korea
    ## 1928                                                           Thailand
    ## 1929                                       Anhui         Mainland China
    ## 1930                                     Beijing         Mainland China
    ## 1931                                   Chongqing         Mainland China
    ## 1932                Diamond Princess cruise ship                 Others
    ## 1933                                      Fujian         Mainland China
    ## 1934                                       Gansu         Mainland China
    ## 1935                                   Guangdong         Mainland China
    ## 1936                                     Guangxi         Mainland China
    ## 1937                                     Guizhou         Mainland China
    ## 1938                                      Hainan         Mainland China
    ## 1939                                       Hebei         Mainland China
    ## 1940                                Heilongjiang         Mainland China
    ## 1941                                       Henan         Mainland China
    ## 1942                                   Hong Kong              Hong Kong
    ## 1943                                       Hubei         Mainland China
    ## 1944                                       Hunan         Mainland China
    ## 1945                              Inner Mongolia         Mainland China
    ## 1946                                     Jiangsu         Mainland China
    ## 1947                                     Jiangxi         Mainland China
    ## 1948                                       Jilin         Mainland China
    ## 1949                                    Liaoning         Mainland China
    ## 1950                                     Ningxia         Mainland China
    ## 1951                                     Shaanxi         Mainland China
    ## 1952                                    Shandong         Mainland China
    ## 1953                                    Shanghai         Mainland China
    ## 1954                                      Shanxi         Mainland China
    ## 1955                                     Sichuan         Mainland China
    ## 1956                                     Tianjin         Mainland China
    ## 1957                                    Xinjiang         Mainland China
    ## 1958                                      Yunnan         Mainland China
    ## 1959                                    Zhejiang         Mainland China
    ## 1960                                                        Afghanistan
    ## 1961                                                            Bahrain
    ## 1962                                                               Iran
    ## 1963                                                               Iraq
    ## 1964                                                              Italy
    ## 1965                                                              Japan
    ## 1966                                                             Kuwait
    ## 1967                                                               Oman
    ## 1968                                                        South Korea
    ## 1969                                                                 UK
    ## 1970                                   Chongqing         Mainland China
    ## 1971                                   Guangdong         Mainland China
    ## 1972                                     Guangxi         Mainland China
    ## 1973                                   Hong Kong              Hong Kong
    ## 1974                                       Hubei         Mainland China
    ## 1975                                       Jilin         Mainland China
    ## 1976        Lackland, TX (From Diamond Princess)                     US
    ## 1977           Omaha, NE (From Diamond Princess)                     US
    ## 1978                                    Shandong         Mainland China
    ## 1979                                      Shanxi         Mainland China
    ## 1980                                     Sichuan         Mainland China
    ## 1981                                      Taiwan                 Taiwan
    ## 1982                                 Toronto, ON                 Canada
    ## 1983          Travis, CA (From Diamond Princess)                     US
    ## 1984 Unassigned Location (From Diamond Princess)                     US
    ## 1985                                                               Iran
    ## 1986                                                              Italy
    ## 1987                                                        South Korea
    ## 1988                                       Hubei         Mainland China
    ## 1989                                    Shandong         Mainland China
    ## 1990                                                              Italy
    ## 1991                                                           Malaysia
    ## 1992                                       Anhui         Mainland China
    ## 1993                                     Beijing         Mainland China
    ## 1994                                   Chongqing         Mainland China
    ## 1995                                      Fujian         Mainland China
    ## 1996                                       Gansu         Mainland China
    ## 1997                                   Guangdong         Mainland China
    ## 1998                                     Guangxi         Mainland China
    ## 1999                                      Hainan         Mainland China
    ## 2000                                       Hebei         Mainland China
    ## 2001                                Heilongjiang         Mainland China
    ## 2002                                       Henan         Mainland China
    ## 2003                                   Hong Kong              Hong Kong
    ## 2004                                       Hubei         Mainland China
    ## 2005                                       Hunan         Mainland China
    ## 2006                              Inner Mongolia         Mainland China
    ## 2007                                     Jiangsu         Mainland China
    ## 2008                                     Jiangxi         Mainland China
    ## 2009                                       Jilin         Mainland China
    ## 2010                                    Liaoning         Mainland China
    ## 2011                                     Ningxia         Mainland China
    ## 2012                                     Shaanxi         Mainland China
    ## 2013                                    Shandong         Mainland China
    ## 2014                                    Shanghai         Mainland China
    ## 2015                                      Shanxi         Mainland China
    ## 2016                                     Sichuan         Mainland China
    ## 2017                                      Taiwan                 Taiwan
    ## 2018                                     Tianjin         Mainland China
    ## 2019                                    Xinjiang         Mainland China
    ## 2020                                      Yunnan         Mainland China
    ## 2021                                    Zhejiang         Mainland China
    ## 2022                                                            Algeria
    ## 2023                                                            Austria
    ## 2024                                                            Bahrain
    ## 2025                                                            Croatia
    ## 2026                                                             France
    ## 2027                                                            Germany
    ## 2028                                                               Iran
    ## 2029                                                              Italy
    ## 2030                                                              Japan
    ## 2031                                                             Kuwait
    ## 2032                                                          Singapore
    ## 2033                                                        South Korea
    ## 2034                                                              Spain
    ## 2035                                                        Switzerland
    ## 2036                                                           Thailand
    ## 2037                                     Beijing         Mainland China
    ## 2038                            British Columbia                 Canada
    ## 2039                                      Fujian         Mainland China
    ## 2040                                   Guangdong         Mainland China
    ## 2041                                     Guangxi         Mainland China
    ## 2042                                   Hong Kong              Hong Kong
    ## 2043                                       Hubei         Mainland China
    ## 2044                                    Shandong         Mainland China
    ## 2045                                    Shanghai         Mainland China
    ## 2046                                     Sichuan         Mainland China
    ## 2047                                      Taiwan                 Taiwan
    ## 2048                                                               Iran
    ## 2049                                                              Italy
    ## 2050                                                        South Korea
    ## 2051                                   Guangdong         Mainland China
    ## 2052                                       Hubei         Mainland China
    ## 2053                                    Shandong         Mainland China
    ## 2054                                                             France
    ## 2055                                                          Singapore
    ## 2056                                                        South Korea
    ## 2057                                                           Thailand
    ## 2058                                                            Vietnam
    ## 2059                                       Anhui         Mainland China
    ## 2060                                     Beijing         Mainland China
    ## 2061                                   Chongqing         Mainland China
    ## 2062                                      Fujian         Mainland China
    ## 2063                                   Guangdong         Mainland China
    ## 2064                                     Guangxi         Mainland China
    ## 2065                                     Guizhou         Mainland China
    ## 2066                                      Hainan         Mainland China
    ## 2067                                       Hebei         Mainland China
    ## 2068                                Heilongjiang         Mainland China
    ## 2069                                       Henan         Mainland China
    ## 2070                                       Hubei         Mainland China
    ## 2071                                       Hunan         Mainland China
    ## 2072                              Inner Mongolia         Mainland China
    ## 2073                                     Jiangsu         Mainland China
    ## 2074                                     Jiangxi         Mainland China
    ## 2075                                       Jilin         Mainland China
    ## 2076                                    Liaoning         Mainland China
    ## 2077                                       Macau                  Macau
    ## 2078                                     Ningxia         Mainland China
    ## 2079                                     Shaanxi         Mainland China
    ## 2080                                    Shandong         Mainland China
    ## 2081                                    Shanghai         Mainland China
    ## 2082                                      Shanxi         Mainland China
    ## 2083                                     Sichuan         Mainland China
    ## 2084                                   Tempe, AZ                     US
    ## 2085                                     Tianjin         Mainland China
    ## 2086                                      Yunnan         Mainland China
    ## 2087                                    Zhejiang         Mainland China
    ## 2088                                                            Bahrain
    ## 2089                                                             Brazil
    ## 2090                                                            Croatia
    ## 2091                                                            Finland
    ## 2092                                                             France
    ## 2093                                                            Georgia
    ## 2094                                                            Germany
    ## 2095                                                             Greece
    ## 2096                                                               Iran
    ## 2097                                                               Iraq
    ## 2098                                                             Israel
    ## 2099                                                              Italy
    ## 2100                                                              Japan
    ## 2101                                                             Kuwait
    ## 2102                                                            Lebanon
    ## 2103                                                    North Macedonia
    ## 2104                                                             Norway
    ## 2105                                                               Oman
    ## 2106                                                           Pakistan
    ## 2107                                                            Romania
    ## 2108                                                          Singapore
    ## 2109                                                        South Korea
    ## 2110                                                              Spain
    ## 2111                                                             Sweden
    ## 2112                                                           Thailand
    ## 2113                Diamond Princess cruise ship                 Others
    ## 2114                                       Hebei         Mainland China
    ## 2115                                   Hong Kong              Hong Kong
    ## 2116                                       Hubei         Mainland China
    ## 2117                                    Shanghai         Mainland China
    ## 2118                                     Sichuan         Mainland China
    ## 2119                                      Taiwan                 Taiwan
    ## 2120 Unassigned Location (From Diamond Princess)                     US
    ## 2121                                                             France
    ## 2122                                                               Iran
    ## 2123                                                              Italy
    ## 2124                                                              Japan
    ## 2125                                                        South Korea
    ## 2126                Diamond Princess cruise ship                 Others
    ## 2127                                       Hubei         Mainland China
    ## 2128                                                            Germany
    ## 2129                                                               Iran
    ## 2130                                                              Italy
    ## 2131                                                          Singapore
    ## 2132                                       Anhui         Mainland China
    ## 2133                                     Beijing         Mainland China
    ## 2134                                   Chongqing         Mainland China
    ## 2135                Diamond Princess cruise ship                 Others
    ## 2136                                      Fujian         Mainland China
    ## 2137                                       Gansu         Mainland China
    ## 2138                                   Guangdong         Mainland China
    ## 2139                                     Guangxi         Mainland China
    ## 2140                                      Hainan         Mainland China
    ## 2141                                       Hebei         Mainland China
    ## 2142                                Heilongjiang         Mainland China
    ## 2143                                       Henan         Mainland China
    ## 2144                                   Hong Kong              Hong Kong
    ## 2145                                       Hubei         Mainland China
    ## 2146                                       Hunan         Mainland China
    ## 2147                              Inner Mongolia         Mainland China
    ## 2148                                     Jiangsu         Mainland China
    ## 2149                                     Jiangxi         Mainland China
    ## 2150                                       Jilin         Mainland China
    ## 2151                                    Liaoning         Mainland China
    ## 2152                                     Ningxia         Mainland China
    ## 2153                                     Shaanxi         Mainland China
    ## 2154                                    Shandong         Mainland China
    ## 2155                                    Shanghai         Mainland China
    ## 2156                                      Shanxi         Mainland China
    ## 2157                                     Sichuan         Mainland China
    ## 2158                                     Tianjin         Mainland China
    ## 2159                                    Xinjiang         Mainland China
    ## 2160                                      Yunnan         Mainland China
    ## 2161                                    Zhejiang         Mainland China
    ## 2162                                                            Austria
    ## 2163                                                            Denmark
    ## 2164                                                            Estonia
    ## 2165                                                             France
    ## 2166                                                            Germany
    ## 2167                                                             Greece
    ## 2168                                                               Iran
    ## 2169                                                               Iraq
    ## 2170                                                             Israel
    ## 2171                                                              Italy
    ## 2172                                                              Japan
    ## 2173                                                             Kuwait
    ## 2174                                                           Malaysia
    ## 2175                                                        Netherlands
    ## 2176                                                         San Marino
    ## 2177                                                        South Korea
    ## 2178                                                              Spain
    ## 2179                                                             Sweden
    ## 2180                                                        Switzerland
    ## 2181                                                                 UK
    ## 2182                                     Beijing         Mainland China
    ## 2183                       From Diamond Princess              Australia
    ## 2184                                      Fujian         Mainland China
    ## 2185                                       Hebei         Mainland China
    ## 2186                                       Henan         Mainland China
    ## 2187                                   Hong Kong              Hong Kong
    ## 2188                                       Hubei         Mainland China
    ## 2189                                       Hunan         Mainland China
    ## 2190                                     Ningxia         Mainland China
    ## 2191                       Sacramento County, CA                     US
    ## 2192                                     Sichuan         Mainland China
    ## 2193                                     Tianjin         Mainland China
    ## 2194                                 Toronto, ON                 Canada
    ## 2195                                                               Iran
    ## 2196                                                              Italy
    ## 2197                                                              Japan
    ## 2198                                                        South Korea
    ## 2199                                     Beijing         Mainland China
    ## 2200                                Heilongjiang         Mainland China
    ## 2201                                       Henan         Mainland China
    ## 2202                                       Hubei         Mainland China
    ## 2203                                                            Germany
    ## 2204                                                             Israel
    ## 2205                                                              Italy
    ## 2206                                       Anhui         Mainland China
    ## 2207                                     Beijing         Mainland China
    ## 2208                            British Columbia                 Canada
    ## 2209                                   Chongqing         Mainland China
    ## 2210                                      Fujian         Mainland China
    ## 2211                                   Guangdong         Mainland China
    ## 2212                                     Guangxi         Mainland China
    ## 2213                                     Guizhou         Mainland China
    ## 2214                                      Hainan         Mainland China
    ## 2215                                       Hebei         Mainland China
    ## 2216                                Heilongjiang         Mainland China
    ## 2217                                       Henan         Mainland China
    ## 2218                                       Hubei         Mainland China
    ## 2219                                       Hunan         Mainland China
    ## 2220                              Inner Mongolia         Mainland China
    ## 2221                                     Jiangsu         Mainland China
    ## 2222                                     Jiangxi         Mainland China
    ## 2223                                       Jilin         Mainland China
    ## 2224                                    Liaoning         Mainland China
    ## 2225                                       Macau                  Macau
    ## 2226                                     Ningxia         Mainland China
    ## 2227                                     Shaanxi         Mainland China
    ## 2228                                    Shandong         Mainland China
    ## 2229                                    Shanghai         Mainland China
    ## 2230                                      Shanxi         Mainland China
    ## 2231                                     Sichuan         Mainland China
    ## 2232                                     Tianjin         Mainland China
    ## 2233                                    Xinjiang         Mainland China
    ## 2234                                      Yunnan         Mainland China
    ## 2235                                    Zhejiang         Mainland China
    ## 2236                                                            Bahrain
    ## 2237                                                            Belarus
    ## 2238                                                            Croatia
    ## 2239                                                             France
    ## 2240                                                            Germany
    ## 2241                                                             Greece
    ## 2242                                                            Iceland
    ## 2243                                                               Iran
    ## 2244                                                             Israel
    ## 2245                                                              Italy
    ## 2246                                                              Japan
    ## 2247                                                             Kuwait
    ## 2248                                                          Lithuania
    ## 2249                                                             Mexico
    ## 2250                                                        New Zealand
    ## 2251                                                            Nigeria
    ## 2252                                                             Norway
    ## 2253                                                            Romania
    ## 2254                                                        South Korea
    ## 2255                                                              Spain
    ## 2256                                                           Thailand
    ## 2257                                                                 UK
    ## 2258                                               United Arab Emirates
    ## 2259                                Montreal, QC                 Canada
    ## 2260                                       Anhui         Mainland China
    ## 2261                                   Guangdong         Mainland China
    ## 2262                                       Hebei         Mainland China
    ## 2263                                   Hong Kong              Hong Kong
    ## 2264                                       Hubei         Mainland China
    ## 2265                                     Jiangxi         Mainland China
    ## 2266                                     Sichuan         Mainland China
    ## 2267                                      Taiwan                 Taiwan
    ## 2268 Unassigned Location (From Diamond Princess)                     US
    ## 2269                                                               Iran
    ## 2270                                                              Italy
    ## 2271                                     Beijing         Mainland China
    ## 2272                Diamond Princess cruise ship                 Others
    ## 2273                                       Hubei         Mainland China
    ## 2274                                    Xinjiang         Mainland China
    ## 2275                                                              Egypt
    ## 2276                                                               Iran
    ## 2277                                                              Italy
    ## 2278                                                           Thailand
    ## 2279                                               United Arab Emirates
    ## 2280                                       Anhui         Mainland China
    ## 2281                                     Beijing         Mainland China
    ## 2282                                  Boston, MA                     US
    ## 2283                                   Chongqing         Mainland China
    ## 2284                                      Fujian         Mainland China
    ## 2285                                       Gansu         Mainland China
    ## 2286                                   Guangdong         Mainland China
    ## 2287                                     Guangxi         Mainland China
    ## 2288                                      Hainan         Mainland China
    ## 2289                                       Hebei         Mainland China
    ## 2290                                Heilongjiang         Mainland China
    ## 2291                                       Henan         Mainland China
    ## 2292                                   Hong Kong              Hong Kong
    ## 2293                                       Hubei         Mainland China
    ## 2294                                       Hunan         Mainland China
    ## 2295                              Inner Mongolia         Mainland China
    ## 2296                                     Jiangsu         Mainland China
    ## 2297                                     Jiangxi         Mainland China
    ## 2298                                       Jilin         Mainland China
    ## 2299                                     Shaanxi         Mainland China
    ## 2300                                    Shandong         Mainland China
    ## 2301                                    Shanghai         Mainland China
    ## 2302                                      Shanxi         Mainland China
    ## 2303                                     Sichuan         Mainland China
    ## 2304                                      Taiwan                 Taiwan
    ## 2305                                    Xinjiang         Mainland China
    ## 2306                                      Yunnan         Mainland China
    ## 2307                                    Zhejiang         Mainland China
    ## 2308                                                            Austria
    ## 2309                                                            Bahrain
    ## 2310                                                             Brazil
    ## 2311                                                            Croatia
    ## 2312                                                            Denmark
    ## 2313                                                            Finland
    ## 2314                                                             France
    ## 2315                                                            Germany
    ## 2316                                                               Iran
    ## 2317                                                               Iraq
    ## 2318                                                            Ireland
    ## 2319                                                             Israel
    ## 2320                                                              Italy
    ## 2321                                                              Japan
    ## 2322                                                            Lebanon
    ## 2323                                                         Luxembourg
    ## 2324                                                           Malaysia
    ## 2325                                                             Mexico
    ## 2326                                                             Monaco
    ## 2327                                                        Netherlands
    ## 2328                                                             Norway
    ## 2329                                                               Oman
    ## 2330                                                           Pakistan
    ## 2331                                                              Qatar
    ## 2332                                                          Singapore
    ## 2333                                                        South Korea
    ## 2334                                                              Spain
    ## 2335                                                             Sweden
    ## 2336                                                        Switzerland
    ## 2337                                                           Thailand
    ## 2338                                                                 UK
    ## 2339                                               United Arab Emirates
    ## 2340                                     Beijing         Mainland China
    ## 2341                            British Columbia                 Canada
    ## 2342                       From Diamond Princess              Australia
    ## 2343                                   Guangdong         Mainland China
    ## 2344                                   Hong Kong              Hong Kong
    ## 2345                                       Hubei         Mainland China
    ## 2346                                       Hunan         Mainland China
    ## 2347                             King County, WA                     US
    ## 2348                                     Ningxia         Mainland China
    ## 2349                                  Queensland              Australia
    ## 2350                             Santa Clara, CA                     US
    ## 2351                        Snohomish County, WA                     US
    ## 2352                             South Australia              Australia
    ## 2353                                      Taiwan                 Taiwan
    ## 2354                                 Toronto, ON                 Canada
    ## 2355                                    Victoria              Australia
    ## 2356                       Washington County, OR                     US
    ## 2357                           Western Australia              Australia
    ## 2358                                                               Iran
    ## 2359                                                              Italy
    ## 2360                                                              Japan
    ## 2361                                                        South Korea
    ## 2362                                     Beijing         Mainland China
    ## 2363                                       Henan         Mainland China
    ## 2364                                       Hubei         Mainland China
    ## 2365                             King County, WA                     US
    ## 2366                                                             France
    ## 2367                                                               Iran
    ## 2368                                                              Japan
    ## 2369                                                               Oman
    ## 2370                                                          Singapore
    ## 2371                                                        South Korea
    ## 2372                                       Anhui         Mainland China
    ## 2373                                     Beijing         Mainland China
    ## 2374                                   Chongqing         Mainland China
    ## 2375                                      Fujian         Mainland China
    ## 2376                                   Guangdong         Mainland China
    ## 2377                                     Guangxi         Mainland China
    ## 2378                                      Hainan         Mainland China
    ## 2379                                       Hebei         Mainland China
    ## 2380                                Heilongjiang         Mainland China
    ## 2381                                       Henan         Mainland China
    ## 2382                                   Hong Kong              Hong Kong
    ## 2383                                       Hubei         Mainland China
    ## 2384                                       Hunan         Mainland China
    ## 2385                              Inner Mongolia         Mainland China
    ## 2386                                     Jiangsu         Mainland China
    ## 2387                                     Jiangxi         Mainland China
    ## 2388                                       Jilin         Mainland China
    ## 2389                                    Liaoning         Mainland China
    ## 2390                                     Ningxia         Mainland China
    ## 2391                                     Shaanxi         Mainland China
    ## 2392                                    Shandong         Mainland China
    ## 2393                                    Shanghai         Mainland China
    ## 2394                                      Shanxi         Mainland China
    ## 2395                                     Sichuan         Mainland China
    ## 2396                                      Taiwan                 Taiwan
    ## 2397                                     Tianjin         Mainland China
    ## 2398                                    Xinjiang         Mainland China
    ## 2399                                      Yunnan         Mainland China
    ## 2400                                    Zhejiang         Mainland China
    ## 2401                                                            Armenia
    ## 2402                                                            Austria
    ## 2403                                                         Azerbaijan
    ## 2404                                                            Bahrain
    ## 2405                                                            Belgium
    ## 2406                                                            Croatia
    ## 2407                                                     Czech Republic
    ## 2408                                                            Denmark
    ## 2409                                                 Dominican Republic
    ## 2410                                                            Ecuador
    ## 2411                                                              Egypt
    ## 2412                                                            Finland
    ## 2413                                                             France
    ## 2414                                                            Georgia
    ## 2415                                                            Germany
    ## 2416                                                             Greece
    ## 2417                                                            Iceland
    ## 2418                                                               Iran
    ## 2419                                                               Iraq
    ## 2420                                                             Israel
    ## 2421                                                              Italy
    ## 2422                                                              Japan
    ## 2423                                                            Lebanon
    ## 2424                                                           Malaysia
    ## 2425                                                             Mexico
    ## 2426                                                        Netherlands
    ## 2427                                                             Norway
    ## 2428                                                              Qatar
    ## 2429                                                          Singapore
    ## 2430                                                        South Korea
    ## 2431                                                              Spain
    ## 2432                                                             Sweden
    ## 2433                                                        Switzerland
    ## 2434                                                                 UK
    ## 2435                                     Beijing         Mainland China
    ## 2436                             Cook County, IL                     US
    ## 2437                                   Hong Kong              Hong Kong
    ## 2438                                       Hubei         Mainland China
    ## 2439                             King County, WA                     US
    ## 2440                                    Liaoning         Mainland China
    ## 2441                             New South Wales              Australia
    ## 2442                              Providence, RI                     US
    ## 2443                                    Shandong         Mainland China
    ## 2444                        Snohomish County, WA                     US
    ## 2445                                      Taiwan                 Taiwan
    ## 2446                                 Toronto, ON                 Canada
    ## 2447                                                               Iran
    ## 2448                                                              Italy
    ## 2449                                                              Japan
    ## 2450                                                        South Korea
    ## 2451                                                           Thailand
    ## 2452                                       Henan         Mainland China
    ## 2453                                       Hubei         Mainland China
    ## 2454                           Western Australia              Australia
    ## 2455                                                               Iran
    ## 2456                                                              Italy
    ## 2457                                                        South Korea
    ## 2458                                       Anhui         Mainland China
    ## 2459                                     Beijing         Mainland China
    ## 2460                                   Chongqing         Mainland China
    ## 2461                                      Fujian         Mainland China
    ## 2462                                       Gansu         Mainland China
    ## 2463                                   Guangdong         Mainland China
    ## 2464                                     Guangxi         Mainland China
    ## 2465                                      Hainan         Mainland China
    ## 2466                                       Hebei         Mainland China
    ## 2467                                Heilongjiang         Mainland China
    ## 2468                                       Henan         Mainland China
    ## 2469                                   Hong Kong              Hong Kong
    ## 2470                                       Hubei         Mainland China
    ## 2471                                       Hunan         Mainland China
    ## 2472                              Inner Mongolia         Mainland China
    ## 2473                                     Jiangsu         Mainland China
    ## 2474                                     Jiangxi         Mainland China
    ## 2475                                       Jilin         Mainland China
    ## 2476                                    Liaoning         Mainland China
    ## 2477                                     Shaanxi         Mainland China
    ## 2478                                    Shandong         Mainland China
    ## 2479                                    Shanghai         Mainland China
    ## 2480                                      Shanxi         Mainland China
    ## 2481                                     Sichuan         Mainland China
    ## 2482                                     Tianjin         Mainland China
    ## 2483                                    Xinjiang         Mainland China
    ## 2484                                      Yunnan         Mainland China
    ## 2485                                    Zhejiang         Mainland China
    ## 2486                                                            Algeria
    ## 2487                                                            Andorra
    ## 2488                                                            Austria
    ## 2489                                                            Bahrain
    ## 2490                                                            Belgium
    ## 2491                                                             France
    ## 2492                                                            Germany
    ## 2493                                                            Iceland
    ## 2494                                                              India
    ## 2495                                                          Indonesia
    ## 2496                                                               Iran
    ## 2497                                                               Iraq
    ## 2498                                                              Italy
    ## 2499                                                              Japan
    ## 2500                                                             Kuwait
    ## 2501                                                             Latvia
    ## 2502                                                            Lebanon
    ## 2503                                                            Morocco
    ## 2504                                                        Netherlands
    ## 2505                                                             Norway
    ## 2506                                                           Portugal
    ## 2507                                                             Russia
    ## 2508                                                         San Marino
    ## 2509                                                       Saudi Arabia
    ## 2510                                                            Senegal
    ## 2511                                                          Singapore
    ## 2512                                                        South Korea
    ## 2513                                                              Spain
    ## 2514                                                             Sweden
    ## 2515                                                        Switzerland
    ## 2516                                                           Thailand
    ## 2517                                                                 UK
    ## 2518                                     Beijing         Mainland China
    ## 2519                             Cook County, IL                     US
    ## 2520                          Grafton County, NH                     US
    ## 2521                                   Guangdong         Mainland China
    ## 2522                            Hillsborough, FL                     US
    ## 2523                                   Hong Kong              Hong Kong
    ## 2524                                       Hubei         Mainland China
    ## 2525                             King County, WA                     US
    ## 2526                           New York City, NY                     US
    ## 2527                                     Ningxia         Mainland China
    ## 2528                           Placer County, CA                     US
    ## 2529                              Providence, RI                     US
    ## 2530                               San Mateo, CA                     US
    ## 2531                             Santa Clara, CA                     US
    ## 2532                                Sarasota, FL                     US
    ## 2533                        Snohomish County, WA                     US
    ## 2534                           Sonoma County, CA                     US
    ## 2535                                      Taiwan                 Taiwan
    ## 2536                                    Tasmania              Australia
    ## 2537                                 Toronto, ON                 Canada
    ## 2538                                Umatilla, OR                     US
    ## 2539 Unassigned Location (From Diamond Princess)                     US
    ## 2540                                    Victoria              Australia
    ## 2541                       Washington County, OR                     US
    ## 2542                                    Zhejiang         Mainland China
    ## 2543                                                             France
    ## 2544                                                               Iran
    ## 2545                                                              Italy
    ## 2546                                                        South Korea
    ## 2547                                       Hubei         Mainland China
    ## 2548                             King County, WA                     US
    ## 2549                        Snohomish County, WA                     US
    ## 2550                                                               Iran
    ## 2551                                                              Italy
    ## 2552                                                          Singapore
    ## 2553                                                           Thailand
    ## 2554                                       Anhui         Mainland China
    ## 2555                                     Beijing         Mainland China
    ## 2556                                   Chongqing         Mainland China
    ## 2557                                      Fujian         Mainland China
    ## 2558                                       Gansu         Mainland China
    ## 2559                                   Guangdong         Mainland China
    ## 2560                                     Guangxi         Mainland China
    ## 2561                                     Guizhou         Mainland China
    ## 2562                                      Hainan         Mainland China
    ## 2563                                       Hebei         Mainland China
    ## 2564                                Heilongjiang         Mainland China
    ## 2565                                       Henan         Mainland China
    ## 2566                                       Hubei         Mainland China
    ## 2567                                       Hunan         Mainland China
    ## 2568                              Inner Mongolia         Mainland China
    ## 2569                                     Jiangsu         Mainland China
    ## 2570                                     Jiangxi         Mainland China
    ## 2571                                       Jilin         Mainland China
    ## 2572                                     Shaanxi         Mainland China
    ## 2573                                    Shandong         Mainland China
    ## 2574                                    Shanghai         Mainland China
    ## 2575                                      Shanxi         Mainland China
    ## 2576                                     Sichuan         Mainland China
    ## 2577                                      Taiwan                 Taiwan
    ## 2578                                    Xinjiang         Mainland China
    ## 2579                                      Yunnan         Mainland China
    ## 2580                                    Zhejiang         Mainland China
    ## 2581                                                            Algeria
    ## 2582                                                          Argentina
    ## 2583                                                            Austria
    ## 2584                                                            Belgium
    ## 2585                                                              Chile
    ## 2586                                                            Croatia
    ## 2587                                                     Czech Republic
    ## 2588                                                            Denmark
    ## 2589                                                            Ecuador
    ## 2590                                                            Estonia
    ## 2591                                                             France
    ## 2592                                                            Germany
    ## 2593                                                            Iceland
    ## 2594                                                               Iran
    ## 2595                                                               Iraq
    ## 2596                                                            Ireland
    ## 2597                                                             Israel
    ## 2598                                                              Italy
    ## 2599                                                              Japan
    ## 2600                                                             Jordan
    ## 2601                                                           Malaysia
    ## 2602                                                        Netherlands
    ## 2603                                                             Norway
    ## 2604                                                               Oman
    ## 2605                                                           Pakistan
    ## 2606                                                              Qatar
    ## 2607                                                         San Marino
    ## 2608                                                            Senegal
    ## 2609                                                          Singapore
    ## 2610                                                        South Korea
    ## 2611                                                              Spain
    ## 2612                                                             Sweden
    ## 2613                                                        Switzerland
    ## 2614                                                                 UK
    ## 2615                                                            Ukraine
    ## 2616                                               United Arab Emirates
    ## 2617                          Norfolk County, MA                     US
    ## 2618                                Berkeley, CA                     US
    ## 2619                            British Columbia                 Canada
    ## 2620                Diamond Princess cruise ship                 Others
    ## 2621                           Fulton County, GA                     US
    ## 2622                          Grafton County, NH                     US
    ## 2623                            Hillsborough, FL                     US
    ## 2624                                       Hubei         Mainland China
    ## 2625                             King County, WA                     US
    ## 2626                                    Liaoning         Mainland China
    ## 2627                         Maricopa County, AZ                     US
    ## 2628                             New South Wales              Australia
    ## 2629                                  Queensland              Australia
    ## 2630                               San Mateo, CA                     US
    ## 2631                             Santa Clara, CA                     US
    ## 2632                                    Shanghai         Mainland China
    ## 2633                        Snohomish County, WA                     US
    ## 2634                                      Taiwan                 Taiwan
    ## 2635                                 Toronto, ON                 Canada
    ## 2636                             Wake County, NC                     US
    ## 2637                      Westchester County, NY                     US
    ## 2638                                    Zhejiang         Mainland China
    ## 2639                                                             France
    ## 2640                                                               Iran
    ## 2641                                                              Italy
    ## 2642                                                         San Marino
    ## 2643                                                              Spain
    ## 2644                                       Hubei         Mainland China
    ## 2645                              Inner Mongolia         Mainland China
    ## 2646                             King County, WA                     US
    ## 2647                                                              Italy
    ## 2648                                                              Japan
    ## 2649                                                           Malaysia
    ## 2650                                                             Mexico
    ## 2651                                                               Oman
    ## 2652                                                        Switzerland
    ## 2653                                       Anhui         Mainland China
    ## 2654                                     Beijing         Mainland China
    ## 2655                                   Chongqing         Mainland China
    ## 2656                                      Fujian         Mainland China
    ## 2657                                       Gansu         Mainland China
    ## 2658                                   Guangdong         Mainland China
    ## 2659                                     Guangxi         Mainland China
    ## 2660                                      Hainan         Mainland China
    ## 2661                                       Hebei         Mainland China
    ## 2662                                Heilongjiang         Mainland China
    ## 2663                                       Henan         Mainland China
    ## 2664                                   Hong Kong              Hong Kong
    ## 2665                                       Hubei         Mainland China
    ## 2666                                       Hunan         Mainland China
    ## 2667                              Inner Mongolia         Mainland China
    ## 2668                                     Jiangsu         Mainland China
    ## 2669                                     Jiangxi         Mainland China
    ## 2670                                    Liaoning         Mainland China
    ## 2671                                       Macau                  Macau
    ## 2672                                 Madison, WI                     US
    ## 2673                                    Shandong         Mainland China
    ## 2674                                    Shanghai         Mainland China
    ## 2675                                      Shanxi         Mainland China
    ## 2676                                     Sichuan         Mainland China
    ## 2677                                     Tianjin         Mainland China
    ## 2678                                    Xinjiang         Mainland China
    ## 2679                                      Yunnan         Mainland China
    ## 2680                                    Zhejiang         Mainland China
    ## 2681                                                            Algeria
    ## 2682                                                            Austria
    ## 2683                                                            Bahrain
    ## 2684                                                            Belarus
    ## 2685                                                            Belgium
    ## 2686                                                             Brazil
    ## 2687                                                            Croatia
    ## 2688                                                     Czech Republic
    ## 2689                                                            Denmark
    ## 2690                                                            Ecuador
    ## 2691                                                      Faroe Islands
    ## 2692                                                             France
    ## 2693                                                            Germany
    ## 2694                                                          Gibraltar
    ## 2695                                                             Greece
    ## 2696                                                            Hungary
    ## 2697                                                            Iceland
    ## 2698                                                              India
    ## 2699                                                               Iran
    ## 2700                                                               Iraq
    ## 2701                                                            Ireland
    ## 2702                                                             Israel
    ## 2703                                                              Italy
    ## 2704                                                              Japan
    ## 2705                                                      Liechtenstein
    ## 2706                                                           Malaysia
    ## 2707                                                        Netherlands
    ## 2708                                                        New Zealand
    ## 2709                                                             Norway
    ## 2710                                                               Oman
    ## 2711                                                             Poland
    ## 2712                                                           Portugal
    ## 2713                                                              Qatar
    ## 2714                                                            Romania
    ## 2715                                                   Saint Barthelemy
    ## 2716                                                         San Marino
    ## 2717                                                            Senegal
    ## 2718                                                        South Korea
    ## 2719                                                              Spain
    ## 2720                                                             Sweden
    ## 2721                                                        Switzerland
    ## 2722                                                            Tunisia
    ## 2723                                                                 UK
    ## 2724                                     Beijing         Mainland China
    ## 2725                            British Columbia                 Canada
    ## 2726                     Contra Costa County, CA                     US
    ## 2727                                   Hong Kong              Hong Kong
    ## 2728                                       Hubei         Mainland China
    ## 2729                             King County, WA                     US
    ## 2730                             Los Angeles, CA                     US
    ## 2731                             New South Wales              Australia
    ## 2732                                     Ningxia         Mainland China
    ## 2733                          Northern Territory              Australia
    ## 2734                           Orange County, CA                     US
    ## 2735                           Placer County, CA                     US
    ## 2736                        Snohomish County, WA                     US
    ## 2737                             South Australia              Australia
    ## 2738                                    Victoria              Australia
    ## 2739                      Westchester County, NY                     US
    ## 2740                                                               Iran
    ## 2741                                                               Iraq
    ## 2742                                                              Italy
    ## 2743                                                        South Korea
    ## 2744                                                              Spain
    ## 2745                                       Hubei         Mainland China
    ## 2746                             King County, WA                     US
    ## 2747                             New South Wales              Australia
    ## 2748                           Placer County, CA                     US
    ## 2749                                                               Iran
    ## 2750                                                              Italy
    ## 2751                                                            Lebanon
    ## 2752                                                            Romania
    ## 2753                                                        South Korea
    ## 2754                                                        Switzerland
    ## 2755                                       Anhui         Mainland China
    ## 2756                                     Beijing         Mainland China
    ## 2757                                   Chongqing         Mainland China
    ## 2758                                      Fujian         Mainland China
    ## 2759                                       Gansu         Mainland China
    ## 2760                                   Guangdong         Mainland China
    ## 2761                                     Guangxi         Mainland China
    ## 2762                                      Hainan         Mainland China
    ## 2763                                       Hebei         Mainland China
    ## 2764                                Heilongjiang         Mainland China
    ## 2765                                       Henan         Mainland China
    ## 2766                                       Hubei         Mainland China
    ## 2767                                       Hunan         Mainland China
    ## 2768                              Inner Mongolia         Mainland China
    ## 2769                                     Jiangsu         Mainland China
    ## 2770                                     Jiangxi         Mainland China
    ## 2771                                       Jilin         Mainland China
    ## 2772                                     Shaanxi         Mainland China
    ## 2773                                    Shandong         Mainland China
    ## 2774                                    Shanghai         Mainland China
    ## 2775                                     Sichuan         Mainland China
    ## 2776                                    Xinjiang         Mainland China
    ## 2777                                    Zhejiang         Mainland China
    ## 2778                                                            Austria
    ## 2779                                                         Azerbaijan
    ## 2780                                                            Bahrain
    ## 2781                                                            Belgium
    ## 2782                                             Bosnia and Herzegovina
    ## 2783                                                              Chile
    ## 2784                                                     Czech Republic
    ## 2785                                                            Ecuador
    ## 2786                                                              Egypt
    ## 2787                                                            Estonia
    ## 2788                                                            Finland
    ## 2789                                                             France
    ## 2790                                                            Georgia
    ## 2791                                                            Germany
    ## 2792                                                             Greece
    ## 2793                                                            Iceland
    ## 2794                                                              India
    ## 2795                                                               Iran
    ## 2796                                                             Israel
    ## 2797                                                              Italy
    ## 2798                                                              Japan
    ## 2799                                                             Kuwait
    ## 2800                                                            Lebanon
    ## 2801                                                            Morocco
    ## 2802                                                        Netherlands
    ## 2803                                                             Norway
    ## 2804                                                               Oman
    ## 2805                                                          Palestine
    ## 2806                                                           Portugal
    ## 2807                                                            Romania
    ## 2808                                                             Russia
    ## 2809                                                         San Marino
    ## 2810                                                       Saudi Arabia
    ## 2811                                                          Singapore
    ## 2812                                                           Slovenia
    ## 2813                                                       South Africa
    ## 2814                                                        South Korea
    ## 2815                                                              Spain
    ## 2816                                                             Sweden
    ## 2817                                                        Switzerland
    ## 2818                                                           Thailand
    ## 2819                                                                 UK
    ## 2820                                               United Arab Emirates
    ## 2821                                Montreal, QC                 Canada
    ## 2822                           Bergen County, NJ                     US
    ## 2823                            British Columbia                 Canada
    ## 2824                            Clark County, NV                     US
    ## 2825                             Cook County, IL                     US
    ## 2826                        Fort Bend County, TX                     US
    ## 2827                                       Gansu         Mainland China
    ## 2828                            Grant County, WA                     US
    ## 2829                                   Guangdong         Mainland China
    ## 2830                           Harris County, TX                     US
    ## 2831                                Heilongjiang         Mainland China
    ## 2832                                       Hubei         Mainland China
    ## 2833                             King County, WA                     US
    ## 2834                             Los Angeles, CA                     US
    ## 2835                           New York City, NY                     US
    ## 2836                           Queens County, NY                     US
    ## 2837                                  Queensland              Australia
    ## 2838                        San Diego County, CA                     US
    ## 2839                    San Francisco County, CA                     US
    ## 2840                             Santa Clara, CA                     US
    ## 2841                       Santa Rosa County, FL                     US
    ## 2842                                    Shanghai         Mainland China
    ## 2843                                     Sichuan         Mainland China
    ## 2844                        Snohomish County, WA                     US
    ## 2845                                      Taiwan                 Taiwan
    ## 2846                                 Toronto, ON                 Canada
    ## 2847                      Westchester County, NY                     US
    ## 2848                           Western Australia              Australia
    ## 2849                       Williamson County, TN                     US
    ## 2850                                    Zhejiang         Mainland China
    ## 2851                                                             France
    ## 2852                                                               Iran
    ## 2853                                                              Italy
    ## 2854                                                              Spain
    ## 2855                                                        Switzerland
    ## 2856                                                                 UK
    ## 2857                                      Hainan         Mainland China
    ## 2858                                       Hubei         Mainland China
    ## 2859                             King County, WA                     US
    ## 2860                                                               Iran
    ## 2861                                                              Italy
    ## 2862                                       Anhui         Mainland China
    ## 2863                                   Chongqing         Mainland China
    ## 2864                                      Fujian         Mainland China
    ## 2865                                   Guangdong         Mainland China
    ## 2866                                     Guangxi         Mainland China
    ## 2867                                       Hebei         Mainland China
    ## 2868                                Heilongjiang         Mainland China
    ## 2869                                       Henan         Mainland China
    ## 2870                                   Hong Kong              Hong Kong
    ## 2871                                       Hubei         Mainland China
    ## 2872                                       Hunan         Mainland China
    ## 2873                              Inner Mongolia         Mainland China
    ## 2874                                     Jiangsu         Mainland China
    ## 2875                                     Jiangxi         Mainland China
    ## 2876                                       Jilin         Mainland China
    ## 2877                                  Queensland              Australia
    ## 2878                                     Shaanxi         Mainland China
    ## 2879                                    Shandong         Mainland China
    ## 2880                                    Shanghai         Mainland China
    ## 2881                                      Shanxi         Mainland China
    ## 2882                                     Sichuan         Mainland China
    ## 2883                                     Tianjin         Mainland China
    ## 2884                                    Victoria              Australia
    ## 2885                                    Xinjiang         Mainland China
    ## 2886                                    Zhejiang         Mainland China
    ##            Lat      Long       date cases      type
    ## 1     36.00000  138.0000 2020-01-22     2 confirmed
    ## 2     36.00000  128.0000 2020-01-22     1 confirmed
    ## 3     15.00000  101.0000 2020-01-22     2 confirmed
    ## 4     31.82570  117.2264 2020-01-22     1 confirmed
    ## 5     40.18240  116.4142 2020-01-22    14 confirmed
    ## 6     30.05720  107.8740 2020-01-22     6 confirmed
    ## 7     26.07890  117.9874 2020-01-22     1 confirmed
    ## 8     23.34170  113.4244 2020-01-22    26 confirmed
    ## 9     23.82980  108.7881 2020-01-22     2 confirmed
    ## 10    26.81540  106.8748 2020-01-22     1 confirmed
    ## 11    19.19590  109.7453 2020-01-22     4 confirmed
    ## 12    38.04280  114.5149 2020-01-22     1 confirmed
    ## 13    33.88202  113.6140 2020-01-22     5 confirmed
    ## 14    30.97560  112.2707 2020-01-22   444 confirmed
    ## 15    27.61040  111.7088 2020-01-22     4 confirmed
    ## 16    32.97110  119.4550 2020-01-22     1 confirmed
    ## 17    27.61400  115.7221 2020-01-22     2 confirmed
    ## 18    47.60620 -122.3321 2020-01-22     1 confirmed
    ## 19    41.29560  122.6085 2020-01-22     2 confirmed
    ## 20    22.16670  113.5500 2020-01-22     1 confirmed
    ## 21    37.26920  106.1655 2020-01-22     1 confirmed
    ## 22    36.34270  118.1498 2020-01-22     2 confirmed
    ## 23    31.20200  121.4491 2020-01-22     9 confirmed
    ## 24    37.57770  112.2922 2020-01-22     1 confirmed
    ## 25    30.61710  102.7103 2020-01-22     5 confirmed
    ## 26    23.70000  121.0000 2020-01-22     1 confirmed
    ## 27    39.30540  117.3230 2020-01-22     4 confirmed
    ## 28    24.97400  101.4870 2020-01-22     1 confirmed
    ## 29    29.18320  120.0934 2020-01-22    10 confirmed
    ## 30    30.97560  112.2707 2020-01-22    17     death
    ## 31    30.97560  112.2707 2020-01-22    28 recovered
    ## 32    36.00000  138.0000 2020-01-23    -1 confirmed
    ## 33     1.28330  103.8333 2020-01-23     1 confirmed
    ## 34    15.00000  101.0000 2020-01-23     1 confirmed
    ## 35    16.00000  108.0000 2020-01-23     2 confirmed
    ## 36    31.82570  117.2264 2020-01-23     8 confirmed
    ## 37    40.18240  116.4142 2020-01-23     8 confirmed
    ## 38    30.05720  107.8740 2020-01-23     3 confirmed
    ## 39    26.07890  117.9874 2020-01-23     4 confirmed
    ## 40    36.06110  103.8343 2020-01-23     2 confirmed
    ## 41    23.34170  113.4244 2020-01-23     6 confirmed
    ## 42    23.82980  108.7881 2020-01-23     3 confirmed
    ## 43    26.81540  106.8748 2020-01-23     2 confirmed
    ## 44    19.19590  109.7453 2020-01-23     1 confirmed
    ## 45    47.86200  127.7615 2020-01-23     2 confirmed
    ## 46    22.30000  114.2000 2020-01-23     2 confirmed
    ## 47    27.61040  111.7088 2020-01-23     5 confirmed
    ## 48    32.97110  119.4550 2020-01-23     4 confirmed
    ## 49    27.61400  115.7221 2020-01-23     5 confirmed
    ## 50    43.66610  126.1923 2020-01-23     1 confirmed
    ## 51    41.29560  122.6085 2020-01-23     1 confirmed
    ## 52    22.16670  113.5500 2020-01-23     1 confirmed
    ## 53    35.19170  108.8701 2020-01-23     3 confirmed
    ## 54    36.34270  118.1498 2020-01-23     4 confirmed
    ## 55    31.20200  121.4491 2020-01-23     7 confirmed
    ## 56    30.61710  102.7103 2020-01-23     3 confirmed
    ## 57    41.11290   85.2401 2020-01-23     2 confirmed
    ## 58    24.97400  101.4870 2020-01-23     1 confirmed
    ## 59    29.18320  120.0934 2020-01-23    17 confirmed
    ## 60    38.04280  114.5149 2020-01-23     1     death
    ## 61    23.34170  113.4244 2020-01-23     2 recovered
    ## 62    47.00000    2.0000 2020-01-24     2 confirmed
    ## 63    36.00000  138.0000 2020-01-24     1 confirmed
    ## 64     1.28330  103.8333 2020-01-24     2 confirmed
    ## 65    36.00000  128.0000 2020-01-24     1 confirmed
    ## 66    15.00000  101.0000 2020-01-24     2 confirmed
    ## 67    31.82570  117.2264 2020-01-24     6 confirmed
    ## 68    40.18240  116.4142 2020-01-24    14 confirmed
    ## 69    30.05720  107.8740 2020-01-24    18 confirmed
    ## 70    41.73770  -87.6976 2020-01-24     1 confirmed
    ## 71    26.07890  117.9874 2020-01-24     5 confirmed
    ## 72    23.34170  113.4244 2020-01-24    21 confirmed
    ## 73    23.82980  108.7881 2020-01-24    18 confirmed
    ## 74    19.19590  109.7453 2020-01-24     3 confirmed
    ## 75    38.04280  114.5149 2020-01-24     1 confirmed
    ## 76    47.86200  127.7615 2020-01-24     2 confirmed
    ## 77    33.88202  113.6140 2020-01-24     4 confirmed
    ## 78    30.97560  112.2707 2020-01-24   105 confirmed
    ## 79    27.61040  111.7088 2020-01-24    15 confirmed
    ## 80    44.09350  113.9448 2020-01-24     1 confirmed
    ## 81    32.97110  119.4550 2020-01-24     4 confirmed
    ## 82    27.61400  115.7221 2020-01-24    11 confirmed
    ## 83    43.66610  126.1923 2020-01-24     2 confirmed
    ## 84    41.29560  122.6085 2020-01-24     1 confirmed
    ## 85    37.26920  106.1655 2020-01-24     1 confirmed
    ## 86    35.19170  108.8701 2020-01-24     2 confirmed
    ## 87    36.34270  118.1498 2020-01-24     9 confirmed
    ## 88    31.20200  121.4491 2020-01-24     4 confirmed
    ## 89    30.61710  102.7103 2020-01-24     7 confirmed
    ## 90    23.70000  121.0000 2020-01-24     2 confirmed
    ## 91    39.30540  117.3230 2020-01-24     4 confirmed
    ## 92    24.97400  101.4870 2020-01-24     3 confirmed
    ## 93    29.18320  120.0934 2020-01-24    16 confirmed
    ## 94    47.86200  127.7615 2020-01-24     1     death
    ## 95    30.97560  112.2707 2020-01-24     7     death
    ## 96    40.18240  116.4142 2020-01-24     1 recovered
    ## 97    30.97560  112.2707 2020-01-24     3 recovered
    ## 98    31.20200  121.4491 2020-01-24     1 recovered
    ## 99    29.18320  120.0934 2020-01-24     1 recovered
    ## 100   47.00000    2.0000 2020-01-25     1 confirmed
    ## 101    2.50000  112.5000 2020-01-25     3 confirmed
    ## 102   28.16670   84.2500 2020-01-25     1 confirmed
    ## 103   15.00000  101.0000 2020-01-25     2 confirmed
    ## 104   31.82570  117.2264 2020-01-25    24 confirmed
    ## 105   40.18240  116.4142 2020-01-25     5 confirmed
    ## 106   30.05720  107.8740 2020-01-25    30 confirmed
    ## 107   26.07890  117.9874 2020-01-25     8 confirmed
    ## 108   36.06110  103.8343 2020-01-25     2 confirmed
    ## 109   23.34170  113.4244 2020-01-25    25 confirmed
    ## 110   26.81540  106.8748 2020-01-25     1 confirmed
    ## 111   19.19590  109.7453 2020-01-25    11 confirmed
    ## 112   38.04280  114.5149 2020-01-25     6 confirmed
    ## 113   47.86200  127.7615 2020-01-25     5 confirmed
    ## 114   33.88202  113.6140 2020-01-25    23 confirmed
    ## 115   22.30000  114.2000 2020-01-25     3 confirmed
    ## 116   30.97560  112.2707 2020-01-25   212 confirmed
    ## 117   27.61040  111.7088 2020-01-25    19 confirmed
    ## 118   44.09350  113.9448 2020-01-25     6 confirmed
    ## 119   32.97110  119.4550 2020-01-25     9 confirmed
    ## 120   43.66610  126.1923 2020-01-25     1 confirmed
    ## 121   41.29560  122.6085 2020-01-25    13 confirmed
    ## 122   37.26920  106.1655 2020-01-25     1 confirmed
    ## 123   35.74520   95.9956 2020-01-25     1 confirmed
    ## 124   35.19170  108.8701 2020-01-25    10 confirmed
    ## 125   36.34270  118.1498 2020-01-25    12 confirmed
    ## 126   31.20200  121.4491 2020-01-25    13 confirmed
    ## 127   37.57770  112.2922 2020-01-25     5 confirmed
    ## 128   30.61710  102.7103 2020-01-25    13 confirmed
    ## 129   39.30540  117.3230 2020-01-25     2 confirmed
    ## 130   41.11290   85.2401 2020-01-25     1 confirmed
    ## 131   24.97400  101.4870 2020-01-25     6 confirmed
    ## 132   29.18320  120.0934 2020-01-25    19 confirmed
    ## 133   30.97560  112.2707 2020-01-25    16     death
    ## 134   40.18240  116.4142 2020-01-25     1 recovered
    ## 135   30.97560  112.2707 2020-01-25     1 recovered
    ## 136   32.97110  119.4550 2020-01-25     1 recovered
    ## 137   36.00000  138.0000 2020-01-26     2 confirmed
    ## 138    2.50000  112.5000 2020-01-26     1 confirmed
    ## 139    1.28330  103.8333 2020-01-26     1 confirmed
    ## 140   36.00000  128.0000 2020-01-26     1 confirmed
    ## 141   15.00000  101.0000 2020-01-26     1 confirmed
    ## 142   31.82570  117.2264 2020-01-26    21 confirmed
    ## 143   40.18240  116.4142 2020-01-26    27 confirmed
    ## 144   30.05720  107.8740 2020-01-26    18 confirmed
    ## 145   26.07890  117.9874 2020-01-26    17 confirmed
    ## 146   36.06110  103.8343 2020-01-26     3 confirmed
    ## 147   23.34170  113.4244 2020-01-26    33 confirmed
    ## 148   23.82980  108.7881 2020-01-26    13 confirmed
    ## 149   26.81540  106.8748 2020-01-26     1 confirmed
    ## 150   19.19590  109.7453 2020-01-26     3 confirmed
    ## 151   38.04280  114.5149 2020-01-26     5 confirmed
    ## 152   47.86200  127.7615 2020-01-26     6 confirmed
    ## 153   33.88202  113.6140 2020-01-26    51 confirmed
    ## 154   22.30000  114.2000 2020-01-26     3 confirmed
    ## 155   30.97560  112.2707 2020-01-26   297 confirmed
    ## 156   27.61040  111.7088 2020-01-26    26 confirmed
    ## 157   32.97110  119.4550 2020-01-26    15 confirmed
    ## 158   27.61400  115.7221 2020-01-26    18 confirmed
    ## 159   41.29560  122.6085 2020-01-26     4 confirmed
    ## 160   34.05220 -118.2437 2020-01-26     1 confirmed
    ## 161   22.16670  113.5500 2020-01-26     3 confirmed
    ## 162  -33.86880  151.2093 2020-01-26     3 confirmed
    ## 163   37.26920  106.1655 2020-01-26     1 confirmed
    ## 164   33.78790 -117.8531 2020-01-26     1 confirmed
    ## 165   35.19170  108.8701 2020-01-26     7 confirmed
    ## 166   36.34270  118.1498 2020-01-26    19 confirmed
    ## 167   31.20200  121.4491 2020-01-26     7 confirmed
    ## 168   37.57770  112.2922 2020-01-26     3 confirmed
    ## 169   30.61710  102.7103 2020-01-26    16 confirmed
    ## 170   23.70000  121.0000 2020-01-26     1 confirmed
    ## 171   33.42550 -111.9400 2020-01-26     1 confirmed
    ## 172   39.30540  117.3230 2020-01-26     4 confirmed
    ## 173   43.65320  -79.3832 2020-01-26     1 confirmed
    ## 174  -37.81360  144.9631 2020-01-26     1 confirmed
    ## 175   41.11290   85.2401 2020-01-26     1 confirmed
    ## 176   24.97400  101.4870 2020-01-26     5 confirmed
    ## 177   29.18320  120.0934 2020-01-26    42 confirmed
    ## 178   33.88202  113.6140 2020-01-26     1     death
    ## 179   30.97560  112.2707 2020-01-26    12     death
    ## 180   31.20200  121.4491 2020-01-26     1     death
    ## 181   36.00000  138.0000 2020-01-26     1 recovered
    ## 182   15.00000  101.0000 2020-01-26     2 recovered
    ## 183   30.97560  112.2707 2020-01-26    10 recovered
    ## 184   11.55000  104.9167 2020-01-27     1 confirmed
    ## 185   51.00000    9.0000 2020-01-27     1 confirmed
    ## 186    1.28330  103.8333 2020-01-27     1 confirmed
    ## 187   36.00000  128.0000 2020-01-27     1 confirmed
    ## 188    7.00000   81.0000 2020-01-27     1 confirmed
    ## 189   31.82570  117.2264 2020-01-27    10 confirmed
    ## 190   40.18240  116.4142 2020-01-27    12 confirmed
    ## 191   30.05720  107.8740 2020-01-27    35 confirmed
    ## 192   26.07890  117.9874 2020-01-27    24 confirmed
    ## 193   36.06110  103.8343 2020-01-27     7 confirmed
    ## 194   23.34170  113.4244 2020-01-27    40 confirmed
    ## 195   23.82980  108.7881 2020-01-27    10 confirmed
    ## 196   26.81540  106.8748 2020-01-27     2 confirmed
    ## 197   19.19590  109.7453 2020-01-27    11 confirmed
    ## 198   38.04280  114.5149 2020-01-27     5 confirmed
    ## 199   47.86200  127.7615 2020-01-27     6 confirmed
    ## 200   33.88202  113.6140 2020-01-27    45 confirmed
    ## 201   30.97560  112.2707 2020-01-27   365 confirmed
    ## 202   27.61040  111.7088 2020-01-27    31 confirmed
    ## 203   44.09350  113.9448 2020-01-27     4 confirmed
    ## 204   32.97110  119.4550 2020-01-27    14 confirmed
    ## 205   27.61400  115.7221 2020-01-27    36 confirmed
    ## 206   43.66610  126.1923 2020-01-27     2 confirmed
    ## 207   41.29560  122.6085 2020-01-27     6 confirmed
    ## 208   22.16670  113.5500 2020-01-27     1 confirmed
    ## 209  -33.86880  151.2093 2020-01-27     1 confirmed
    ## 210   37.26920  106.1655 2020-01-27     3 confirmed
    ## 211   35.74520   95.9956 2020-01-27     5 confirmed
    ## 212   35.19170  108.8701 2020-01-27    13 confirmed
    ## 213   36.34270  118.1498 2020-01-27    29 confirmed
    ## 214   31.20200  121.4491 2020-01-27    13 confirmed
    ## 215   37.57770  112.2922 2020-01-27     4 confirmed
    ## 216   30.61710  102.7103 2020-01-27    25 confirmed
    ## 217   23.70000  121.0000 2020-01-27     1 confirmed
    ## 218   39.30540  117.3230 2020-01-27     9 confirmed
    ## 219   41.11290   85.2401 2020-01-27     1 confirmed
    ## 220   24.97400  101.4870 2020-01-27    10 confirmed
    ## 221   29.18320  120.0934 2020-01-27    24 confirmed
    ## 222   40.18240  116.4142 2020-01-27     1     death
    ## 223   19.19590  109.7453 2020-01-27     1     death
    ## 224   30.97560  112.2707 2020-01-27    24     death
    ## 225   23.34170  113.4244 2020-01-27     2 recovered
    ## 226   30.97560  112.2707 2020-01-27     3 recovered
    ## 227   27.61400  115.7221 2020-01-27     2 recovered
    ## 228   31.20200  121.4491 2020-01-27     2 recovered
    ## 229   47.00000    2.0000 2020-01-28     1 confirmed
    ## 230   51.00000    9.0000 2020-01-28     3 confirmed
    ## 231   36.00000  138.0000 2020-01-28     3 confirmed
    ## 232    1.28330  103.8333 2020-01-28     2 confirmed
    ## 233   15.00000  101.0000 2020-01-28     6 confirmed
    ## 234   31.82570  117.2264 2020-01-28    36 confirmed
    ## 235   40.18240  116.4142 2020-01-28    11 confirmed
    ## 236   49.28270 -123.1207 2020-01-28     1 confirmed
    ## 237   30.05720  107.8740 2020-01-28    22 confirmed
    ## 238   26.07890  117.9874 2020-01-28    21 confirmed
    ## 239   36.06110  103.8343 2020-01-28     5 confirmed
    ## 240   23.34170  113.4244 2020-01-28    56 confirmed
    ## 241   23.82980  108.7881 2020-01-28     5 confirmed
    ## 242   26.81540  106.8748 2020-01-28     2 confirmed
    ## 243   19.19590  109.7453 2020-01-28     7 confirmed
    ## 244   38.04280  114.5149 2020-01-28    15 confirmed
    ## 245   47.86200  127.7615 2020-01-28    12 confirmed
    ## 246   33.88202  113.6140 2020-01-28    40 confirmed
    ## 247   30.97560  112.2707 2020-01-28  2131 confirmed
    ## 248   27.61040  111.7088 2020-01-28    43 confirmed
    ## 249   44.09350  113.9448 2020-01-28     4 confirmed
    ## 250   32.97110  119.4550 2020-01-28    23 confirmed
    ## 251   27.61400  115.7221 2020-01-28    37 confirmed
    ## 252   43.66610  126.1923 2020-01-28     2 confirmed
    ## 253   41.29560  122.6085 2020-01-28     7 confirmed
    ## 254   22.16670  113.5500 2020-01-28     1 confirmed
    ## 255   37.26920  106.1655 2020-01-28     4 confirmed
    ## 256   35.19170  108.8701 2020-01-28    11 confirmed
    ## 257   36.34270  118.1498 2020-01-28    20 confirmed
    ## 258   31.20200  121.4491 2020-01-28    13 confirmed
    ## 259   37.57770  112.2922 2020-01-28    14 confirmed
    ## 260   30.61710  102.7103 2020-01-28    21 confirmed
    ## 261   23.70000  121.0000 2020-01-28     3 confirmed
    ## 262   39.30540  117.3230 2020-01-28     1 confirmed
    ## 263   41.11290   85.2401 2020-01-28     5 confirmed
    ## 264   24.97400  101.4870 2020-01-28    18 confirmed
    ## 265   29.18320  120.0934 2020-01-28    45 confirmed
    ## 266   30.97560  112.2707 2020-01-28    49     death
    ## 267   15.00000  101.0000 2020-01-28     3 recovered
    ## 268   40.18240  116.4142 2020-01-28     2 recovered
    ## 269   23.82980  108.7881 2020-01-28     2 recovered
    ## 270   30.97560  112.2707 2020-01-28    35 recovered
    ## 271   27.61400  115.7221 2020-01-28     1 recovered
    ## 272   31.20200  121.4491 2020-01-28     1 recovered
    ## 273   29.18320  120.0934 2020-01-28     2 recovered
    ## 274   64.00000   26.0000 2020-01-29     1 confirmed
    ## 275   47.00000    2.0000 2020-01-29     1 confirmed
    ## 276    2.50000  112.5000 2020-01-29     3 confirmed
    ## 277   24.00000   54.0000 2020-01-29     4 confirmed
    ## 278   31.82570  117.2264 2020-01-29    46 confirmed
    ## 279   40.18240  116.4142 2020-01-29    20 confirmed
    ## 280   30.05720  107.8740 2020-01-29    15 confirmed
    ## 281   26.07890  117.9874 2020-01-29     4 confirmed
    ## 282   36.06110  103.8343 2020-01-29     5 confirmed
    ## 283   23.34170  113.4244 2020-01-29    70 confirmed
    ## 284   23.82980  108.7881 2020-01-29     7 confirmed
    ## 285   19.19590  109.7453 2020-01-29     3 confirmed
    ## 286   38.04280  114.5149 2020-01-29    15 confirmed
    ## 287   47.86200  127.7615 2020-01-29     5 confirmed
    ## 288   33.88202  113.6140 2020-01-29    38 confirmed
    ## 289   22.30000  114.2000 2020-01-29     2 confirmed
    ## 290   27.61040  111.7088 2020-01-29    78 confirmed
    ## 291   44.09350  113.9448 2020-01-29     1 confirmed
    ## 292   32.97110  119.4550 2020-01-29    29 confirmed
    ## 293   43.66610  126.1923 2020-01-29     1 confirmed
    ## 294   41.29560  122.6085 2020-01-29     5 confirmed
    ## 295   37.26920  106.1655 2020-01-29     1 confirmed
    ## 296  -28.01670  153.4000 2020-01-29     1 confirmed
    ## 297   35.19170  108.8701 2020-01-29    10 confirmed
    ## 298   36.34270  118.1498 2020-01-29    35 confirmed
    ## 299   31.20200  121.4491 2020-01-29    30 confirmed
    ## 300   30.61710  102.7103 2020-01-29    18 confirmed
    ## 301   39.30540  117.3230 2020-01-29     3 confirmed
    ## 302   41.11290   85.2401 2020-01-29     3 confirmed
    ## 303   24.97400  101.4870 2020-01-29    11 confirmed
    ## 304   29.18320  120.0934 2020-01-29   123 confirmed
    ## 305   33.88202  113.6140 2020-01-29     1     death
    ## 306   30.61710  102.7103 2020-01-29     1     death
    ## 307   31.82570  117.2264 2020-01-29     2 recovered
    ## 308   30.05720  107.8740 2020-01-29     1 recovered
    ## 309   23.34170  113.4244 2020-01-29     1 recovered
    ## 310   26.81540  106.8748 2020-01-29     1 recovered
    ## 311   33.88202  113.6140 2020-01-29     1 recovered
    ## 312   30.97560  112.2707 2020-01-29     8 recovered
    ## 313   41.29560  122.6085 2020-01-29     1 recovered
    ## 314   36.34270  118.1498 2020-01-29     1 recovered
    ## 315   31.20200  121.4491 2020-01-29     1 recovered
    ## 316   37.57770  112.2922 2020-01-29     1 recovered
    ## 317   30.61710  102.7103 2020-01-29     1 recovered
    ## 318   21.00000   78.0000 2020-01-30     1 confirmed
    ## 319   36.00000  138.0000 2020-01-30     4 confirmed
    ## 320    2.50000  112.5000 2020-01-30     1 confirmed
    ## 321   13.00000  122.0000 2020-01-30     1 confirmed
    ## 322    1.28330  103.8333 2020-01-30     3 confirmed
    ## 323   31.82570  117.2264 2020-01-30    48 confirmed
    ## 324   40.18240  116.4142 2020-01-30     3 confirmed
    ## 325   30.05720  107.8740 2020-01-30    35 confirmed
    ## 326   26.07890  117.9874 2020-01-30    17 confirmed
    ## 327   36.06110  103.8343 2020-01-30     2 confirmed
    ## 328   23.34170  113.4244 2020-01-30    77 confirmed
    ## 329   23.82980  108.7881 2020-01-30    20 confirmed
    ## 330   26.81540  106.8748 2020-01-30     3 confirmed
    ## 331   19.19590  109.7453 2020-01-30     3 confirmed
    ## 332   38.04280  114.5149 2020-01-30    17 confirmed
    ## 333   47.86200  127.7615 2020-01-30     6 confirmed
    ## 334   33.88202  113.6140 2020-01-30    72 confirmed
    ## 335   30.97560  112.2707 2020-01-30  1349 confirmed
    ## 336   27.61040  111.7088 2020-01-30    56 confirmed
    ## 337   44.09350  113.9448 2020-01-30     3 confirmed
    ## 338   32.97110  119.4550 2020-01-30    30 confirmed
    ## 339   27.61400  115.7221 2020-01-30    53 confirmed
    ## 340   43.66610  126.1923 2020-01-30     5 confirmed
    ## 341   41.29560  122.6085 2020-01-30     2 confirmed
    ## 342   37.26920  106.1655 2020-01-30     5 confirmed
    ## 343   35.74520   95.9956 2020-01-30     2 confirmed
    ## 344  -28.01670  153.4000 2020-01-30     2 confirmed
    ## 345   35.19170  108.8701 2020-01-30     7 confirmed
    ## 346   36.34270  118.1498 2020-01-30    28 confirmed
    ## 347   31.20200  121.4491 2020-01-30    16 confirmed
    ## 348   37.57770  112.2922 2020-01-30     8 confirmed
    ## 349   30.61710  102.7103 2020-01-30    34 confirmed
    ## 350   23.70000  121.0000 2020-01-30     1 confirmed
    ## 351   39.30540  117.3230 2020-01-30     4 confirmed
    ## 352   31.69270   88.0924 2020-01-30     1 confirmed
    ## 353  -37.81360  144.9631 2020-01-30     1 confirmed
    ## 354   41.11290   85.2401 2020-01-30     1 confirmed
    ## 355   24.97400  101.4870 2020-01-30    15 confirmed
    ## 356   29.18320  120.0934 2020-01-30   132 confirmed
    ## 357   47.86200  127.7615 2020-01-30     1     death
    ## 358   30.97560  112.2707 2020-01-30    37     death
    ## 359   23.34170  113.4244 2020-01-30     5 recovered
    ## 360   19.19590  109.7453 2020-01-30     1 recovered
    ## 361   33.88202  113.6140 2020-01-30     1 recovered
    ## 362   30.97560  112.2707 2020-01-30     2 recovered
    ## 363   27.61040  111.7088 2020-01-30     2 recovered
    ## 364   27.61400  115.7221 2020-01-30     2 recovered
    ## 365   43.66610  126.1923 2020-01-30     1 recovered
    ## 366  -33.86880  151.2093 2020-01-30     2 recovered
    ## 367   29.18320  120.0934 2020-01-30     1 recovered
    ## 368   51.00000    9.0000 2020-01-31     1 confirmed
    ## 369   43.00000   12.0000 2020-01-31     2 confirmed
    ## 370   36.00000  138.0000 2020-01-31     4 confirmed
    ## 371   60.00000   90.0000 2020-01-31     2 confirmed
    ## 372    1.28330  103.8333 2020-01-31     3 confirmed
    ## 373   36.00000  128.0000 2020-01-31     7 confirmed
    ## 374   63.00000   16.0000 2020-01-31     1 confirmed
    ## 375   15.00000  101.0000 2020-01-31     5 confirmed
    ## 376   55.00000   -3.0000 2020-01-31     2 confirmed
    ## 377   31.82570  117.2264 2020-01-31    37 confirmed
    ## 378   40.18240  116.4142 2020-01-31    25 confirmed
    ## 379   30.05720  107.8740 2020-01-31    29 confirmed
    ## 380   41.73770  -87.6976 2020-01-31     1 confirmed
    ## 381   26.07890  117.9874 2020-01-31    19 confirmed
    ## 382   36.06110  103.8343 2020-01-31     3 confirmed
    ## 383   23.34170  113.4244 2020-01-31    82 confirmed
    ## 384   23.82980  108.7881 2020-01-31     9 confirmed
    ## 385   26.81540  106.8748 2020-01-31    17 confirmed
    ## 386   19.19590  109.7453 2020-01-31     6 confirmed
    ## 387   38.04280  114.5149 2020-01-31    17 confirmed
    ## 388   47.86200  127.7615 2020-01-31    15 confirmed
    ## 389   33.88202  113.6140 2020-01-31    74 confirmed
    ## 390   22.30000  114.2000 2020-01-31     2 confirmed
    ## 391   30.97560  112.2707 2020-01-31   903 confirmed
    ## 392   27.61040  111.7088 2020-01-31    55 confirmed
    ## 393   44.09350  113.9448 2020-01-31     1 confirmed
    ## 394   32.97110  119.4550 2020-01-31    39 confirmed
    ## 395   27.61400  115.7221 2020-01-31    78 confirmed
    ## 396   41.29560  122.6085 2020-01-31     7 confirmed
    ## 397   42.98490  -81.2453 2020-01-31     1 confirmed
    ## 398   37.26920  106.1655 2020-01-31     4 confirmed
    ## 399  -28.01670  153.4000 2020-01-31    -1 confirmed
    ## 400   37.35410 -121.9552 2020-01-31     1 confirmed
    ## 401   35.19170  108.8701 2020-01-31    24 confirmed
    ## 402   36.34270  118.1498 2020-01-31    26 confirmed
    ## 403   31.20200  121.4491 2020-01-31    23 confirmed
    ## 404   37.57770  112.2922 2020-01-31     4 confirmed
    ## 405   30.61710  102.7103 2020-01-31    35 confirmed
    ## 406   23.70000  121.0000 2020-01-31     1 confirmed
    ## 407   39.30540  117.3230 2020-01-31     1 confirmed
    ## 408   43.65320  -79.3832 2020-01-31     1 confirmed
    ## 409  -37.81360  144.9631 2020-01-31     1 confirmed
    ## 410   41.11290   85.2401 2020-01-31     3 confirmed
    ## 411   24.97400  101.4870 2020-01-31    13 confirmed
    ## 412   29.18320  120.0934 2020-01-31   110 confirmed
    ## 413   30.97560  112.2707 2020-01-31    42     death
    ## 414   31.82570  117.2264 2020-01-31     1 recovered
    ## 415   40.18240  116.4142 2020-01-31     1 recovered
    ## 416   23.34170  113.4244 2020-01-31     1 recovered
    ## 417   26.81540  106.8748 2020-01-31     1 recovered
    ## 418   33.88202  113.6140 2020-01-31     1 recovered
    ## 419   30.97560  112.2707 2020-01-31    51 recovered
    ## 420   44.09350  113.9448 2020-01-31     1 recovered
    ## 421   32.97110  119.4550 2020-01-31     4 recovered
    ## 422   27.61400  115.7221 2020-01-31     2 recovered
    ## 423   36.34270  118.1498 2020-01-31     1 recovered
    ## 424   31.20200  121.4491 2020-01-31     4 recovered
    ## 425   24.97400  101.4870 2020-01-31     1 recovered
    ## 426   29.18320  120.0934 2020-01-31    10 recovered
    ## 427   47.00000    2.0000 2020-02-01     1 confirmed
    ## 428   51.00000    9.0000 2020-02-01     3 confirmed
    ## 429   36.00000  138.0000 2020-02-01     5 confirmed
    ## 430    1.28330  103.8333 2020-02-01     3 confirmed
    ## 431   36.00000  128.0000 2020-02-01     1 confirmed
    ## 432   40.00000   -4.0000 2020-02-01     1 confirmed
    ## 433   16.00000  108.0000 2020-02-01     4 confirmed
    ## 434   31.82570  117.2264 2020-02-01    60 confirmed
    ## 435   40.18240  116.4142 2020-02-01    29 confirmed
    ## 436   42.36010  -71.0589 2020-02-01     1 confirmed
    ## 437   30.05720  107.8740 2020-02-01    36 confirmed
    ## 438   26.07890  117.9874 2020-02-01    24 confirmed
    ## 439   36.06110  103.8343 2020-02-01    11 confirmed
    ## 440   23.34170  113.4244 2020-02-01    99 confirmed
    ## 441   23.82980  108.7881 2020-02-01    13 confirmed
    ## 442   19.19590  109.7453 2020-02-01    10 confirmed
    ## 443   38.04280  114.5149 2020-02-01    14 confirmed
    ## 444   47.86200  127.7615 2020-02-01    21 confirmed
    ## 445   33.88202  113.6140 2020-02-01    70 confirmed
    ## 446   22.30000  114.2000 2020-02-01     1 confirmed
    ## 447   30.97560  112.2707 2020-02-01  1347 confirmed
    ## 448   27.61040  111.7088 2020-02-01    57 confirmed
    ## 449   44.09350  113.9448 2020-02-01     3 confirmed
    ## 450   32.97110  119.4550 2020-02-01    34 confirmed
    ## 451   27.61400  115.7221 2020-02-01    46 confirmed
    ## 452   43.66610  126.1923 2020-02-01     3 confirmed
    ## 453   41.29560  122.6085 2020-02-01    16 confirmed
    ## 454   37.26920  106.1655 2020-02-01     5 confirmed
    ## 455   35.74520   95.9956 2020-02-01     1 confirmed
    ## 456  -28.01670  153.4000 2020-02-01     1 confirmed
    ## 457   35.19170  108.8701 2020-02-01    14 confirmed
    ## 458   36.34270  118.1498 2020-02-01    22 confirmed
    ## 459   31.20200  121.4491 2020-02-01    34 confirmed
    ## 460   37.57770  112.2922 2020-02-01     8 confirmed
    ## 461   30.61710  102.7103 2020-02-01    30 confirmed
    ## 462  -34.92850  138.6007 2020-02-01     1 confirmed
    ## 463   39.30540  117.3230 2020-02-01     9 confirmed
    ## 464  -37.81360  144.9631 2020-02-01     1 confirmed
    ## 465   41.11290   85.2401 2020-02-01     1 confirmed
    ## 466   24.97400  101.4870 2020-02-01    10 confirmed
    ## 467   29.18320  120.0934 2020-02-01    61 confirmed
    ## 468   30.05720  107.8740 2020-02-01     1     death
    ## 469   30.97560  112.2707 2020-02-01    45     death
    ## 470   16.00000  108.0000 2020-02-01     1 recovered
    ## 471   31.82570  117.2264 2020-02-01     2 recovered
    ## 472   40.18240  116.4142 2020-02-01     4 recovered
    ## 473   30.05720  107.8740 2020-02-01     2 recovered
    ## 474   23.34170  113.4244 2020-02-01     3 recovered
    ## 475   47.86200  127.7615 2020-02-01     2 recovered
    ## 476   30.97560  112.2707 2020-02-01    27 recovered
    ## 477   27.61040  111.7088 2020-02-01     6 recovered
    ## 478   32.97110  119.4550 2020-02-01     1 recovered
    ## 479   27.61400  115.7221 2020-02-01     2 recovered
    ## 480   36.34270  118.1498 2020-02-01     1 recovered
    ## 481   31.20200  121.4491 2020-02-01     1 recovered
    ## 482   30.61710  102.7103 2020-02-01     2 recovered
    ## 483   24.97400  101.4870 2020-02-01     1 recovered
    ## 484   29.18320  120.0934 2020-02-01     7 recovered
    ## 485   51.00000    9.0000 2020-02-02     2 confirmed
    ## 486   21.00000   78.0000 2020-02-02     1 confirmed
    ## 487   13.00000  122.0000 2020-02-02     1 confirmed
    ## 488    1.28330  103.8333 2020-02-02     2 confirmed
    ## 489   36.00000  128.0000 2020-02-02     3 confirmed
    ## 490   24.00000   54.0000 2020-02-02     1 confirmed
    ## 491   31.82570  117.2264 2020-02-02    43 confirmed
    ## 492   40.18240  116.4142 2020-02-02    23 confirmed
    ## 493   30.05720  107.8740 2020-02-02    53 confirmed
    ## 494   26.07890  117.9874 2020-02-02    15 confirmed
    ## 495   36.06110  103.8343 2020-02-02    11 confirmed
    ## 496   23.34170  113.4244 2020-02-02    97 confirmed
    ## 497   23.82980  108.7881 2020-02-02    11 confirmed
    ## 498   26.81540  106.8748 2020-02-02     9 confirmed
    ## 499   19.19590  109.7453 2020-02-02     2 confirmed
    ## 500   38.04280  114.5149 2020-02-02     8 confirmed
    ## 501   47.86200  127.7615 2020-02-02    15 confirmed
    ## 502   33.88202  113.6140 2020-02-02    71 confirmed
    ## 503   22.30000  114.2000 2020-02-02     2 confirmed
    ## 504   30.97560  112.2707 2020-02-02  4024 confirmed
    ## 505   27.61040  111.7088 2020-02-02    74 confirmed
    ## 506   44.09350  113.9448 2020-02-02     4 confirmed
    ## 507   32.97110  119.4550 2020-02-02    34 confirmed
    ## 508   27.61400  115.7221 2020-02-02    47 confirmed
    ## 509   43.66610  126.1923 2020-02-02     6 confirmed
    ## 510   41.29560  122.6085 2020-02-02     6 confirmed
    ## 511   22.16670  113.5500 2020-02-02     1 confirmed
    ## 512   37.26920  106.1655 2020-02-02     2 confirmed
    ## 513   35.74520   95.9956 2020-02-02     2 confirmed
    ## 514  -28.01670  153.4000 2020-02-02    -1 confirmed
    ## 515   35.19170  108.8701 2020-02-02    15 confirmed
    ## 516   36.34270  118.1498 2020-02-02    24 confirmed
    ## 517   31.20200  121.4491 2020-02-02    13 confirmed
    ## 518   37.57770  112.2922 2020-02-02    19 confirmed
    ## 519   30.61710  102.7103 2020-02-02    24 confirmed
    ## 520  -34.92850  138.6007 2020-02-02     1 confirmed
    ## 521   39.30540  117.3230 2020-02-02     7 confirmed
    ## 522   41.11290   85.2401 2020-02-02     3 confirmed
    ## 523   24.97400  101.4870 2020-02-02    12 confirmed
    ## 524   29.18320  120.0934 2020-02-02    62 confirmed
    ## 525   13.00000  122.0000 2020-02-02     1     death
    ## 526   30.05720  107.8740 2020-02-02     1     death
    ## 527   30.97560  112.2707 2020-02-02   101     death
    ## 528   31.82570  117.2264 2020-02-02     2 recovered
    ## 529   30.05720  107.8740 2020-02-02     4 recovered
    ## 530   36.06110  103.8343 2020-02-02     3 recovered
    ## 531   23.34170  113.4244 2020-02-02     1 recovered
    ## 532   19.19590  109.7453 2020-02-02     3 recovered
    ## 533   38.04280  114.5149 2020-02-02     3 recovered
    ## 534   33.88202  113.6140 2020-02-02     7 recovered
    ## 535   30.97560  112.2707 2020-02-02   127 recovered
    ## 536   27.61040  111.7088 2020-02-02     8 recovered
    ## 537   32.97110  119.4550 2020-02-02     1 recovered
    ## 538   27.61400  115.7221 2020-02-02     3 recovered
    ## 539   36.34270  118.1498 2020-02-02     3 recovered
    ## 540   37.57770  112.2922 2020-02-02     2 recovered
    ## 541   30.61710  102.7103 2020-02-02     8 recovered
    ## 542   39.30540  117.3230 2020-02-02     1 recovered
    ## 543   24.97400  101.4870 2020-02-02     1 recovered
    ## 544   29.18320  120.0934 2020-02-02    11 recovered
    ## 545   51.00000    9.0000 2020-02-03     2 confirmed
    ## 546   21.00000   78.0000 2020-02-03     1 confirmed
    ## 547   16.00000  108.0000 2020-02-03     2 confirmed
    ## 548   31.82570  117.2264 2020-02-03    68 confirmed
    ## 549   40.18240  116.4142 2020-02-03    21 confirmed
    ## 550   30.05720  107.8740 2020-02-03    37 confirmed
    ## 551   26.07890  117.9874 2020-02-03    20 confirmed
    ## 552   36.06110  103.8343 2020-02-03     4 confirmed
    ## 553   23.34170  113.4244 2020-02-03    93 confirmed
    ## 554   23.82980  108.7881 2020-02-03    16 confirmed
    ## 555   26.81540  106.8748 2020-02-03     8 confirmed
    ## 556   19.19590  109.7453 2020-02-03     8 confirmed
    ## 557   38.04280  114.5149 2020-02-03     9 confirmed
    ## 558   47.86200  127.7615 2020-02-03    26 confirmed
    ## 559   33.88202  113.6140 2020-02-03    73 confirmed
    ## 560   30.97560  112.2707 2020-02-03  2345 confirmed
    ## 561   27.61040  111.7088 2020-02-03    58 confirmed
    ## 562   44.09350  113.9448 2020-02-03     7 confirmed
    ## 563   32.97110  119.4550 2020-02-03    35 confirmed
    ## 564   27.61400  115.7221 2020-02-03    58 confirmed
    ## 565   43.66610  126.1923 2020-02-03     8 confirmed
    ## 566   41.29560  122.6085 2020-02-03     4 confirmed
    ## 567   37.26920  106.1655 2020-02-03     3 confirmed
    ## 568   35.74520   95.9956 2020-02-03     2 confirmed
    ## 569   36.57610 -120.9876 2020-02-03     2 confirmed
    ## 570   37.35410 -121.9552 2020-02-03     1 confirmed
    ## 571   35.19170  108.8701 2020-02-03    12 confirmed
    ## 572   36.34270  118.1498 2020-02-03    29 confirmed
    ## 573   31.20200  121.4491 2020-02-03    21 confirmed
    ## 574   37.57770  112.2922 2020-02-03     8 confirmed
    ## 575   30.61710  102.7103 2020-02-03    23 confirmed
    ## 576   39.30540  117.3230 2020-02-03    12 confirmed
    ## 577   41.11290   85.2401 2020-02-03     3 confirmed
    ## 578   24.97400  101.4870 2020-02-03    12 confirmed
    ## 579   29.18320  120.0934 2020-02-03    63 confirmed
    ## 580   30.97560  112.2707 2020-02-03    64     death
    ## 581   31.82570  117.2264 2020-02-03     7 recovered
    ## 582   40.18240  116.4142 2020-02-03     3 recovered
    ## 583   30.05720  107.8740 2020-02-03     2 recovered
    ## 584   26.07890  117.9874 2020-02-03     1 recovered
    ## 585   23.34170  113.4244 2020-02-03     6 recovered
    ## 586   23.82980  108.7881 2020-02-03     5 recovered
    ## 587   33.88202  113.6140 2020-02-03     6 recovered
    ## 588   30.97560  112.2707 2020-02-03    91 recovered
    ## 589   27.61040  111.7088 2020-02-03     6 recovered
    ## 590   32.97110  119.4550 2020-02-03     1 recovered
    ## 591   27.61400  115.7221 2020-02-03     6 recovered
    ## 592   37.26920  106.1655 2020-02-03     1 recovered
    ## 593   36.34270  118.1498 2020-02-03     1 recovered
    ## 594   37.57770  112.2922 2020-02-03    -1 recovered
    ## 595   30.61710  102.7103 2020-02-03     3 recovered
    ## 596   24.97400  101.4870 2020-02-03     2 recovered
    ## 597   29.18320  120.0934 2020-02-03    11 recovered
    ## 598   50.83330    4.0000 2020-02-04     1 confirmed
    ## 599   36.00000  138.0000 2020-02-04     2 confirmed
    ## 600    2.50000  112.5000 2020-02-04     2 confirmed
    ## 601    1.28330  103.8333 2020-02-04     6 confirmed
    ## 602   36.00000  128.0000 2020-02-04     1 confirmed
    ## 603   15.00000  101.0000 2020-02-04     6 confirmed
    ## 604   31.82570  117.2264 2020-02-04    72 confirmed
    ## 605   40.18240  116.4142 2020-02-04    16 confirmed
    ## 606   30.05720  107.8740 2020-02-04    29 confirmed
    ## 607   26.07890  117.9874 2020-02-04    15 confirmed
    ## 608   36.06110  103.8343 2020-02-04     2 confirmed
    ## 609   23.34170  113.4244 2020-02-04    88 confirmed
    ## 610   23.82980  108.7881 2020-02-04    12 confirmed
    ## 611   26.81540  106.8748 2020-02-04    12 confirmed
    ## 612   19.19590  109.7453 2020-02-04     8 confirmed
    ## 613   38.04280  114.5149 2020-02-04    13 confirmed
    ## 614   47.86200  127.7615 2020-02-04    34 confirmed
    ## 615   33.88202  113.6140 2020-02-04   109 confirmed
    ## 616   22.30000  114.2000 2020-02-04     2 confirmed
    ## 617   30.97560  112.2707 2020-02-04  3156 confirmed
    ## 618   27.61040  111.7088 2020-02-04    72 confirmed
    ## 619   44.09350  113.9448 2020-02-04     1 confirmed
    ## 620   32.97110  119.4550 2020-02-04    37 confirmed
    ## 621   27.61400  115.7221 2020-02-04    85 confirmed
    ## 622   43.66610  126.1923 2020-02-04    11 confirmed
    ## 623   41.29560  122.6085 2020-02-04     7 confirmed
    ## 624   22.16670  113.5500 2020-02-04     2 confirmed
    ## 625   37.26920  106.1655 2020-02-04     3 confirmed
    ## 626   35.74520   95.9956 2020-02-04     2 confirmed
    ## 627  -28.01670  153.4000 2020-02-04     1 confirmed
    ## 628   35.19170  108.8701 2020-02-04    14 confirmed
    ## 629   36.34270  118.1498 2020-02-04    16 confirmed
    ## 630   31.20200  121.4491 2020-02-04    16 confirmed
    ## 631   37.57770  112.2922 2020-02-04     7 confirmed
    ## 632   30.61710  102.7103 2020-02-04    28 confirmed
    ## 633   23.70000  121.0000 2020-02-04     1 confirmed
    ## 634   39.30540  117.3230 2020-02-04     7 confirmed
    ## 635   41.11290   85.2401 2020-02-04     5 confirmed
    ## 636   24.97400  101.4870 2020-02-04     5 confirmed
    ## 637   29.18320  120.0934 2020-02-04   105 confirmed
    ## 638   22.30000  114.2000 2020-02-04     1     death
    ## 639   30.97560  112.2707 2020-02-04    65     death
    ## 640   31.82570  117.2264 2020-02-04     6 recovered
    ## 641   40.18240  116.4142 2020-02-04    11 recovered
    ## 642   26.07890  117.9874 2020-02-04     2 recovered
    ## 643   36.06110  103.8343 2020-02-04     1 recovered
    ## 644   23.34170  113.4244 2020-02-04     9 recovered
    ## 645   23.82980  108.7881 2020-02-04     3 recovered
    ## 646   19.19590  109.7453 2020-02-04     1 recovered
    ## 647   38.04280  114.5149 2020-02-04     1 recovered
    ## 648   47.86200  127.7615 2020-02-04     2 recovered
    ## 649   33.88202  113.6140 2020-02-04    11 recovered
    ## 650   30.97560  112.2707 2020-02-04   136 recovered
    ## 651   27.61040  111.7088 2020-02-04     9 recovered
    ## 652   32.97110  119.4550 2020-02-04     4 recovered
    ## 653   27.61400  115.7221 2020-02-04     2 recovered
    ## 654   41.29560  122.6085 2020-02-04     1 recovered
    ## 655   35.19170  108.8701 2020-02-04     2 recovered
    ## 656   36.34270  118.1498 2020-02-04     4 recovered
    ## 657   31.20200  121.4491 2020-02-04     2 recovered
    ## 658   37.57770  112.2922 2020-02-04     2 recovered
    ## 659   39.30540  117.3230 2020-02-04     1 recovered
    ## 660   29.18320  120.0934 2020-02-04    19 recovered
    ## 661    2.50000  112.5000 2020-02-05     2 confirmed
    ## 662    1.28330  103.8333 2020-02-05     4 confirmed
    ## 663   36.00000  128.0000 2020-02-05     3 confirmed
    ## 664   31.82570  117.2264 2020-02-05    50 confirmed
    ## 665   40.18240  116.4142 2020-02-05    25 confirmed
    ## 666   49.28270 -123.1207 2020-02-05     1 confirmed
    ## 667   30.05720  107.8740 2020-02-05    23 confirmed
    ## 668   26.07890  117.9874 2020-02-05    11 confirmed
    ## 669   36.06110  103.8343 2020-02-05     5 confirmed
    ## 670   23.34170  113.4244 2020-02-05    82 confirmed
    ## 671   23.82980  108.7881 2020-02-05    11 confirmed
    ## 672   26.81540  106.8748 2020-02-05     6 confirmed
    ## 673   19.19590  109.7453 2020-02-05    19 confirmed
    ## 674   38.04280  114.5149 2020-02-05     9 confirmed
    ## 675   47.86200  127.7615 2020-02-05    35 confirmed
    ## 676   33.88202  113.6140 2020-02-05    89 confirmed
    ## 677   22.30000  114.2000 2020-02-05     4 confirmed
    ## 678   30.97560  112.2707 2020-02-05  2987 confirmed
    ## 679   27.61040  111.7088 2020-02-05    68 confirmed
    ## 680   44.09350  113.9448 2020-02-05     7 confirmed
    ## 681   32.97110  119.4550 2020-02-05    33 confirmed
    ## 682   27.61400  115.7221 2020-02-05    72 confirmed
    ## 683   43.66610  126.1923 2020-02-05    12 confirmed
    ## 684   41.29560  122.6085 2020-02-05     8 confirmed
    ## 685   43.07310  -89.4012 2020-02-05     1 confirmed
    ## 686   35.74520   95.9956 2020-02-05     2 confirmed
    ## 687   35.19170  108.8701 2020-02-05    23 confirmed
    ## 688   36.34270  118.1498 2020-02-05    32 confirmed
    ## 689   31.20200  121.4491 2020-02-05    24 confirmed
    ## 690   30.61710  102.7103 2020-02-05    19 confirmed
    ## 691   39.30540  117.3230 2020-02-05     2 confirmed
    ## 692   41.11290   85.2401 2020-02-05     3 confirmed
    ## 693   24.97400  101.4870 2020-02-05     6 confirmed
    ## 694   29.18320  120.0934 2020-02-05    66 confirmed
    ## 695   26.81540  106.8748 2020-02-05     1     death
    ## 696   30.97560  112.2707 2020-02-05    70     death
    ## 697   39.30540  117.3230 2020-02-05     1     death
    ## 698   31.82570  117.2264 2020-02-05     3 recovered
    ## 699   40.18240  116.4142 2020-02-05     1 recovered
    ## 700   30.05720  107.8740 2020-02-05     6 recovered
    ## 701   26.07890  117.9874 2020-02-05     8 recovered
    ## 702   36.06110  103.8343 2020-02-05     2 recovered
    ## 703   23.34170  113.4244 2020-02-05    19 recovered
    ## 704   23.82980  108.7881 2020-02-05     3 recovered
    ## 705   26.81540  106.8748 2020-02-05     7 recovered
    ## 706   38.04280  114.5149 2020-02-05     2 recovered
    ## 707   47.86200  127.7615 2020-02-05     3 recovered
    ## 708   33.88202  113.6140 2020-02-05    20 recovered
    ## 709   30.97560  112.2707 2020-02-05   111 recovered
    ## 710   27.61040  111.7088 2020-02-05    23 recovered
    ## 711   44.09350  113.9448 2020-02-05     2 recovered
    ## 712   32.97110  119.4550 2020-02-05    11 recovered
    ## 713   27.61400  115.7221 2020-02-05     7 recovered
    ## 714   43.66610  126.1923 2020-02-05     1 recovered
    ## 715   41.29560  122.6085 2020-02-05     2 recovered
    ## 716   35.74520   95.9956 2020-02-05     3 recovered
    ## 717   35.19170  108.8701 2020-02-05     4 recovered
    ## 718   36.34270  118.1498 2020-02-05     4 recovered
    ## 719   31.20200  121.4491 2020-02-05     3 recovered
    ## 720   37.57770  112.2922 2020-02-05     1 recovered
    ## 721   30.61710  102.7103 2020-02-05    10 recovered
    ## 722   29.18320  120.0934 2020-02-05    16 recovered
    ## 723   36.00000  138.0000 2020-02-06    23 confirmed
    ## 724   36.00000  128.0000 2020-02-06     4 confirmed
    ## 725   16.00000  108.0000 2020-02-06     2 confirmed
    ## 726   31.82570  117.2264 2020-02-06    61 confirmed
    ## 727   40.18240  116.4142 2020-02-06    21 confirmed
    ## 728   30.05720  107.8740 2020-02-06    22 confirmed
    ## 729   26.07890  117.9874 2020-02-06    10 confirmed
    ## 730   23.34170  113.4244 2020-02-06    75 confirmed
    ## 731   23.82980  108.7881 2020-02-06    18 confirmed
    ## 732   26.81540  106.8748 2020-02-06     7 confirmed
    ## 733   19.19590  109.7453 2020-02-06     7 confirmed
    ## 734   38.04280  114.5149 2020-02-06    22 confirmed
    ## 735   47.86200  127.7615 2020-02-06    37 confirmed
    ## 736   33.88202  113.6140 2020-02-06    87 confirmed
    ## 737   22.30000  114.2000 2020-02-06     3 confirmed
    ## 738   30.97560  112.2707 2020-02-06  2447 confirmed
    ## 739   27.61040  111.7088 2020-02-06    50 confirmed
    ## 740   44.09350  113.9448 2020-02-06     4 confirmed
    ## 741   32.97110  119.4550 2020-02-06    32 confirmed
    ## 742   27.61400  115.7221 2020-02-06    52 confirmed
    ## 743   43.66610  126.1923 2020-02-06     5 confirmed
    ## 744   41.29560  122.6085 2020-02-06     5 confirmed
    ## 745   37.26920  106.1655 2020-02-06     6 confirmed
    ## 746   35.74520   95.9956 2020-02-06     1 confirmed
    ## 747  -28.01670  153.4000 2020-02-06     1 confirmed
    ## 748   35.19170  108.8701 2020-02-06     8 confirmed
    ## 749   36.34270  118.1498 2020-02-06    40 confirmed
    ## 750   31.20200  121.4491 2020-02-06    14 confirmed
    ## 751   37.57770  112.2922 2020-02-06    15 confirmed
    ## 752   30.61710  102.7103 2020-02-06    20 confirmed
    ## 753   23.70000  121.0000 2020-02-06     5 confirmed
    ## 754   39.30540  117.3230 2020-02-06    10 confirmed
    ## 755   41.11290   85.2401 2020-02-06     4 confirmed
    ## 756   24.97400  101.4870 2020-02-06     5 confirmed
    ## 757   29.18320  120.0934 2020-02-06    59 confirmed
    ## 758   47.86200  127.7615 2020-02-06     1     death
    ## 759   30.97560  112.2707 2020-02-06    69     death
    ## 760   31.82570  117.2264 2020-02-06    11 recovered
    ## 761   40.18240  116.4142 2020-02-06     7 recovered
    ## 762   30.05720  107.8740 2020-02-06     9 recovered
    ## 763   26.07890  117.9874 2020-02-06     3 recovered
    ## 764   23.34170  113.4244 2020-02-06    20 recovered
    ## 765   23.82980  108.7881 2020-02-06     1 recovered
    ## 766   26.81540  106.8748 2020-02-06    -3 recovered
    ## 767   19.19590  109.7453 2020-02-06     3 recovered
    ## 768   38.04280  114.5149 2020-02-06     7 recovered
    ## 769   47.86200  127.7615 2020-02-06     1 recovered
    ## 770   33.88202  113.6140 2020-02-06     9 recovered
    ## 771   30.97560  112.2707 2020-02-06   184 recovered
    ## 772   27.61040  111.7088 2020-02-06    27 recovered
    ## 773   44.09350  113.9448 2020-02-06     1 recovered
    ## 774   32.97110  119.4550 2020-02-06    11 recovered
    ## 775   27.61400  115.7221 2020-02-06    10 recovered
    ## 776   43.66610  126.1923 2020-02-06     2 recovered
    ## 777   41.29560  122.6085 2020-02-06     1 recovered
    ## 778   22.16670  113.5500 2020-02-06     1 recovered
    ## 779   35.19170  108.8701 2020-02-06     3 recovered
    ## 780   36.34270  118.1498 2020-02-06    12 recovered
    ## 781   31.20200  121.4491 2020-02-06    10 recovered
    ## 782   37.57770  112.2922 2020-02-06     7 recovered
    ## 783   30.61710  102.7103 2020-02-06     7 recovered
    ## 784   23.70000  121.0000 2020-02-06     1 recovered
    ## 785   24.97400  101.4870 2020-02-06     2 recovered
    ## 786   29.18320  120.0934 2020-02-06    16 recovered
    ## 787   51.00000    9.0000 2020-02-07     1 confirmed
    ## 788   43.00000   12.0000 2020-02-07     1 confirmed
    ## 789   36.00000  138.0000 2020-02-07   -20 confirmed
    ## 790   13.00000  122.0000 2020-02-07     1 confirmed
    ## 791    1.28330  103.8333 2020-02-07     2 confirmed
    ## 792   36.00000  128.0000 2020-02-07     1 confirmed
    ## 793   55.00000   -3.0000 2020-02-07     1 confirmed
    ## 794   31.82570  117.2264 2020-02-07    74 confirmed
    ## 795   40.18240  116.4142 2020-02-07    23 confirmed
    ## 796   49.28270 -123.1207 2020-02-07     2 confirmed
    ## 797   30.05720  107.8740 2020-02-07    15 confirmed
    ## 798   35.44370  139.6380 2020-02-07    61 confirmed
    ## 799   26.07890  117.9874 2020-02-07     9 confirmed
    ## 800   36.06110  103.8343 2020-02-07     5 confirmed
    ## 801   23.34170  113.4244 2020-02-07    64 confirmed
    ## 802   23.82980  108.7881 2020-02-07     4 confirmed
    ## 803   26.81540  106.8748 2020-02-07    10 confirmed
    ## 804   19.19590  109.7453 2020-02-07    11 confirmed
    ## 805   38.04280  114.5149 2020-02-07    15 confirmed
    ## 806   47.86200  127.7615 2020-02-07    50 confirmed
    ## 807   33.88202  113.6140 2020-02-07    63 confirmed
    ## 808   22.30000  114.2000 2020-02-07     1 confirmed
    ## 809   30.97560  112.2707 2020-02-07  2841 confirmed
    ## 810   27.61040  111.7088 2020-02-07    61 confirmed
    ## 811   44.09350  113.9448 2020-02-07     4 confirmed
    ## 812   32.97110  119.4550 2020-02-07    35 confirmed
    ## 813   27.61400  115.7221 2020-02-07    61 confirmed
    ## 814   43.66610  126.1923 2020-02-07     6 confirmed
    ## 815   41.29560  122.6085 2020-02-07     5 confirmed
    ## 816   37.26920  106.1655 2020-02-07     3 confirmed
    ## 817  -28.01670  153.4000 2020-02-07     1 confirmed
    ## 818   35.19170  108.8701 2020-02-07    11 confirmed
    ## 819   36.34270  118.1498 2020-02-07    39 confirmed
    ## 820   31.20200  121.4491 2020-02-07    20 confirmed
    ## 821   37.57770  112.2922 2020-02-07     8 confirmed
    ## 822   30.61710  102.7103 2020-02-07    23 confirmed
    ## 823   39.30540  117.3230 2020-02-07     2 confirmed
    ## 824   41.11290   85.2401 2020-02-07     3 confirmed
    ## 825   24.97400  101.4870 2020-02-07     5 confirmed
    ## 826   29.18320  120.0934 2020-02-07    52 confirmed
    ## 827   23.34170  113.4244 2020-02-07     1     death
    ## 828   19.19590  109.7453 2020-02-07     1     death
    ## 829   33.88202  113.6140 2020-02-07     1     death
    ## 830   30.97560  112.2707 2020-02-07    81     death
    ## 831   43.66610  126.1923 2020-02-07     1     death
    ## 832    2.50000  112.5000 2020-02-07     1 recovered
    ## 833   36.00000  128.0000 2020-02-07     1 recovered
    ## 834   31.82570  117.2264 2020-02-07    13 recovered
    ## 835   40.18240  116.4142 2020-02-07     2 recovered
    ## 836   30.05720  107.8740 2020-02-07     7 recovered
    ## 837   26.07890  117.9874 2020-02-07     6 recovered
    ## 838   36.06110  103.8343 2020-02-07     3 recovered
    ## 839   23.34170  113.4244 2020-02-07    19 recovered
    ## 840   23.82980  108.7881 2020-02-07     3 recovered
    ## 841   19.19590  109.7453 2020-02-07     2 recovered
    ## 842   38.04280  114.5149 2020-02-07     9 recovered
    ## 843   47.86200  127.7615 2020-02-07     4 recovered
    ## 844   33.88202  113.6140 2020-02-07    30 recovered
    ## 845   30.97560  112.2707 2020-02-07   298 recovered
    ## 846   27.61040  111.7088 2020-02-07    31 recovered
    ## 847   44.09350  113.9448 2020-02-07     1 recovered
    ## 848   32.97110  119.4550 2020-02-07     9 recovered
    ## 849   27.61400  115.7221 2020-02-07     8 recovered
    ## 850   41.29560  122.6085 2020-02-07     2 recovered
    ## 851   37.26920  106.1655 2020-02-07     4 recovered
    ## 852   35.19170  108.8701 2020-02-07     8 recovered
    ## 853   36.34270  118.1498 2020-02-07    10 recovered
    ## 854   31.20200  121.4491 2020-02-07     5 recovered
    ## 855   37.57770  112.2922 2020-02-07     3 recovered
    ## 856   30.61710  102.7103 2020-02-07    11 recovered
    ## 857   24.97400  101.4870 2020-02-07     5 recovered
    ## 858   29.18320  120.0934 2020-02-07    29 recovered
    ## 859   47.00000    2.0000 2020-02-08     5 confirmed
    ## 860    2.50000  112.5000 2020-02-08     4 confirmed
    ## 861    1.28330  103.8333 2020-02-08     3 confirmed
    ## 862   15.00000  101.0000 2020-02-08     7 confirmed
    ## 863   24.00000   54.0000 2020-02-08     2 confirmed
    ## 864   16.00000  108.0000 2020-02-08     3 confirmed
    ## 865   31.82570  117.2264 2020-02-08    68 confirmed
    ## 866   40.18240  116.4142 2020-02-08    18 confirmed
    ## 867   30.05720  107.8740 2020-02-08     2 confirmed
    ## 868   26.07890  117.9874 2020-02-08    15 confirmed
    ## 869   36.06110  103.8343 2020-02-08    12 confirmed
    ## 870   23.34170  113.4244 2020-02-08    61 confirmed
    ## 871   23.82980  108.7881 2020-02-08    11 confirmed
    ## 872   26.81540  106.8748 2020-02-08     8 confirmed
    ## 873   19.19590  109.7453 2020-02-08     7 confirmed
    ## 874   38.04280  114.5149 2020-02-08    23 confirmed
    ## 875   47.86200  127.7615 2020-02-08    18 confirmed
    ## 876   33.88202  113.6140 2020-02-08    67 confirmed
    ## 877   22.30000  114.2000 2020-02-08     1 confirmed
    ## 878   30.97560  112.2707 2020-02-08  2147 confirmed
    ## 879   27.61040  111.7088 2020-02-08    31 confirmed
    ## 880   44.09350  113.9448 2020-02-08     2 confirmed
    ## 881   32.97110  119.4550 2020-02-08    31 confirmed
    ## 882   27.61400  115.7221 2020-02-08    37 confirmed
    ## 883   43.66610  126.1923 2020-02-08     4 confirmed
    ## 884   41.29560  122.6085 2020-02-08     6 confirmed
    ## 885   37.26920  106.1655 2020-02-08     2 confirmed
    ## 886   35.19170  108.8701 2020-02-08    11 confirmed
    ## 887   36.34270  118.1498 2020-02-08    30 confirmed
    ## 888   31.20200  121.4491 2020-02-08     9 confirmed
    ## 889   37.57770  112.2922 2020-02-08    11 confirmed
    ## 890   30.61710  102.7103 2020-02-08    20 confirmed
    ## 891   23.70000  121.0000 2020-02-08     1 confirmed
    ## 892   39.30540  117.3230 2020-02-08     7 confirmed
    ## 893   41.11290   85.2401 2020-02-08     3 confirmed
    ## 894   29.18320  120.0934 2020-02-08    42 confirmed
    ## 895   40.18240  116.4142 2020-02-08     1     death
    ## 896   36.06110  103.8343 2020-02-08     1     death
    ## 897   47.86200  127.7615 2020-02-08     2     death
    ## 898   33.88202  113.6140 2020-02-08     1     death
    ## 899   30.97560  112.2707 2020-02-08    81     death
    ## 900   27.61040  111.7088 2020-02-08     1     death
    ## 901    1.28330  103.8333 2020-02-08     2 recovered
    ## 902    7.00000   81.0000 2020-02-08     1 recovered
    ## 903   15.00000  101.0000 2020-02-08     5 recovered
    ## 904   31.82570  117.2264 2020-02-08    12 recovered
    ## 905   40.18240  116.4142 2020-02-08     1 recovered
    ## 906   30.05720  107.8740 2020-02-08     8 recovered
    ## 907   26.07890  117.9874 2020-02-08     4 recovered
    ## 908   36.06110  103.8343 2020-02-08     3 recovered
    ## 909   23.34170  113.4244 2020-02-08    24 recovered
    ## 910   26.81540  106.8748 2020-02-08     1 recovered
    ## 911   19.19590  109.7453 2020-02-08     4 recovered
    ## 912   38.04280  114.5149 2020-02-08     8 recovered
    ## 913   47.86200  127.7615 2020-02-08     1 recovered
    ## 914   33.88202  113.6140 2020-02-08    30 recovered
    ## 915   30.97560  112.2707 2020-02-08   324 recovered
    ## 916   27.61040  111.7088 2020-02-08    44 recovered
    ## 917   32.97110  119.4550 2020-02-08     8 recovered
    ## 918   27.61400  115.7221 2020-02-08    10 recovered
    ## 919   41.29560  122.6085 2020-02-08     1 recovered
    ## 920   37.26920  106.1655 2020-02-08    10 recovered
    ## 921   35.19170  108.8701 2020-02-08     3 recovered
    ## 922   36.34270  118.1498 2020-02-08     7 recovered
    ## 923   31.20200  121.4491 2020-02-08    11 recovered
    ## 924   37.57770  112.2922 2020-02-08     6 recovered
    ## 925   30.61710  102.7103 2020-02-08    18 recovered
    ## 926   39.30540  117.3230 2020-02-08     2 recovered
    ## 927   24.97400  101.4870 2020-02-08     5 recovered
    ## 928   29.18320  120.0934 2020-02-08    52 recovered
    ## 929   51.00000    9.0000 2020-02-09     1 confirmed
    ## 930   36.00000  138.0000 2020-02-09     1 confirmed
    ## 931    1.28330  103.8333 2020-02-09     7 confirmed
    ## 932   36.00000  128.0000 2020-02-09     1 confirmed
    ## 933   40.00000   -4.0000 2020-02-09     1 confirmed
    ## 934   31.82570  117.2264 2020-02-09    46 confirmed
    ## 935   40.18240  116.4142 2020-02-09    11 confirmed
    ## 936   30.05720  107.8740 2020-02-09    40 confirmed
    ## 937   35.44370  139.6380 2020-02-09     3 confirmed
    ## 938   26.07890  117.9874 2020-02-09    11 confirmed
    ## 939   36.06110  103.8343 2020-02-09     4 confirmed
    ## 940   23.34170  113.4244 2020-02-09    36 confirmed
    ## 941   23.82980  108.7881 2020-02-09    12 confirmed
    ## 942   26.81540  106.8748 2020-02-09    10 confirmed
    ## 943   19.19590  109.7453 2020-02-09     7 confirmed
    ## 944   38.04280  114.5149 2020-02-09    11 confirmed
    ## 945   47.86200  127.7615 2020-02-09    12 confirmed
    ## 946   33.88202  113.6140 2020-02-09    52 confirmed
    ## 947   22.30000  114.2000 2020-02-09     3 confirmed
    ## 948   30.97560  112.2707 2020-02-09  2531 confirmed
    ## 949   27.61040  111.7088 2020-02-09    35 confirmed
    ## 950   44.09350  113.9448 2020-02-09     2 confirmed
    ## 951   32.97110  119.4550 2020-02-09    29 confirmed
    ## 952   27.61400  115.7221 2020-02-09    42 confirmed
    ## 953   43.66610  126.1923 2020-02-09     9 confirmed
    ## 954   41.29560  122.6085 2020-02-09     2 confirmed
    ## 955   35.19170  108.8701 2020-02-09    13 confirmed
    ## 956   36.34270  118.1498 2020-02-09    28 confirmed
    ## 957   31.20200  121.4491 2020-02-09     7 confirmed
    ## 958   37.57770  112.2922 2020-02-09     4 confirmed
    ## 959   30.61710  102.7103 2020-02-09    22 confirmed
    ## 960   23.70000  121.0000 2020-02-09     1 confirmed
    ## 961   39.30540  117.3230 2020-02-09     3 confirmed
    ## 962   41.11290   85.2401 2020-02-09     3 confirmed
    ## 963   24.97400  101.4870 2020-02-09     3 confirmed
    ## 964   29.18320  120.0934 2020-02-09    27 confirmed
    ## 965   31.82570  117.2264 2020-02-09     1     death
    ## 966   36.06110  103.8343 2020-02-09     1     death
    ## 967   23.82980  108.7881 2020-02-09     1     death
    ## 968   19.19590  109.7453 2020-02-09     1     death
    ## 969   38.04280  114.5149 2020-02-09     1     death
    ## 970   47.86200  127.7615 2020-02-09     1     death
    ## 971   33.88202  113.6140 2020-02-09     2     death
    ## 972   30.97560  112.2707 2020-02-09    91     death
    ## 973   36.34270  118.1498 2020-02-09     1     death
    ## 974   36.00000  128.0000 2020-02-09     2 recovered
    ## 975   31.82570  117.2264 2020-02-09    13 recovered
    ## 976   40.18240  116.4142 2020-02-09     3 recovered
    ## 977   30.05720  107.8740 2020-02-09    12 recovered
    ## 978   41.73770  -87.6976 2020-02-09     2 recovered
    ## 979   26.07890  117.9874 2020-02-09    11 recovered
    ## 980   36.06110  103.8343 2020-02-09     4 recovered
    ## 981   23.34170  113.4244 2020-02-09    29 recovered
    ## 982   23.82980  108.7881 2020-02-09     1 recovered
    ## 983   19.19590  109.7453 2020-02-09     5 recovered
    ## 984   38.04280  114.5149 2020-02-09     4 recovered
    ## 985   47.86200  127.7615 2020-02-09     1 recovered
    ## 986   33.88202  113.6140 2020-02-09    37 recovered
    ## 987   30.97560  112.2707 2020-02-09   356 recovered
    ## 988   27.61040  111.7088 2020-02-09    30 recovered
    ## 989   32.97110  119.4550 2020-02-09    20 recovered
    ## 990   27.61400  115.7221 2020-02-09    18 recovered
    ## 991   43.66610  126.1923 2020-02-09     8 recovered
    ## 992   47.60620 -122.3321 2020-02-09     1 recovered
    ## 993   41.29560  122.6085 2020-02-09     4 recovered
    ## 994   37.26920  106.1655 2020-02-09    -2 recovered
    ## 995   35.19170  108.8701 2020-02-09     5 recovered
    ## 996   36.34270  118.1498 2020-02-09    19 recovered
    ## 997   31.20200  121.4491 2020-02-09     3 recovered
    ## 998   37.57770  112.2922 2020-02-09     4 recovered
    ## 999   30.61710  102.7103 2020-02-09    11 recovered
    ## 1000  24.97400  101.4870 2020-02-09     1 recovered
    ## 1001  29.18320  120.0934 2020-02-09    26 recovered
    ## 1002   2.50000  112.5000 2020-02-10     2 confirmed
    ## 1003   1.28330  103.8333 2020-02-10     5 confirmed
    ## 1004  36.00000  128.0000 2020-02-10     2 confirmed
    ## 1005  55.00000   -3.0000 2020-02-10     5 confirmed
    ## 1006  24.00000   54.0000 2020-02-10     1 confirmed
    ## 1007  16.00000  108.0000 2020-02-10     1 confirmed
    ## 1008  31.82570  117.2264 2020-02-10    51 confirmed
    ## 1009  40.18240  116.4142 2020-02-10    11 confirmed
    ## 1010  30.05720  107.8740 2020-02-10    18 confirmed
    ## 1011  35.44370  139.6380 2020-02-10    71 confirmed
    ## 1012  26.07890  117.9874 2020-02-10    11 confirmed
    ## 1013  23.34170  113.4244 2020-02-10    28 confirmed
    ## 1014  23.82980  108.7881 2020-02-10    15 confirmed
    ## 1015  26.81540  106.8748 2020-02-10    10 confirmed
    ## 1016  19.19590  109.7453 2020-02-10     7 confirmed
    ## 1017  38.04280  114.5149 2020-02-10    12 confirmed
    ## 1018  47.86200  127.7615 2020-02-10    24 confirmed
    ## 1019  33.88202  113.6140 2020-02-10    40 confirmed
    ## 1020  22.30000  114.2000 2020-02-10     9 confirmed
    ## 1021  30.97560  112.2707 2020-02-10  2097 confirmed
    ## 1022  27.61040  111.7088 2020-02-10    41 confirmed
    ## 1023  44.09350  113.9448 2020-02-10     4 confirmed
    ## 1024  32.97110  119.4550 2020-02-10    24 confirmed
    ## 1025  27.61400  115.7221 2020-02-10    31 confirmed
    ## 1026  43.66610  126.1923 2020-02-10     2 confirmed
    ## 1027  41.29560  122.6085 2020-02-10     1 confirmed
    ## 1028  37.26920  106.1655 2020-02-10     4 confirmed
    ## 1029  35.19170  108.8701 2020-02-10     5 confirmed
    ## 1030  36.34270  118.1498 2020-02-10    22 confirmed
    ## 1031  31.20200  121.4491 2020-02-10     6 confirmed
    ## 1032  30.61710  102.7103 2020-02-10    19 confirmed
    ## 1033  39.30540  117.3230 2020-02-10     4 confirmed
    ## 1034  41.11290   85.2401 2020-02-10     4 confirmed
    ## 1035  24.97400  101.4870 2020-02-10     8 confirmed
    ## 1036  29.18320  120.0934 2020-02-10    17 confirmed
    ## 1037  31.82570  117.2264 2020-02-10     2     death
    ## 1038  47.86200  127.7615 2020-02-10     1     death
    ## 1039  30.97560  112.2707 2020-02-10   103     death
    ## 1040  27.61400  115.7221 2020-02-10     1     death
    ## 1041  36.00000  138.0000 2020-02-10     3 recovered
    ## 1042  31.82570  117.2264 2020-02-10    16 recovered
    ## 1043  40.18240  116.4142 2020-02-10     7 recovered
    ## 1044  30.05720  107.8740 2020-02-10    15 recovered
    ## 1045  26.07890  117.9874 2020-02-10     4 recovered
    ## 1046  36.06110  103.8343 2020-02-10     1 recovered
    ## 1047  23.34170  113.4244 2020-02-10    26 recovered
    ## 1048  23.82980  108.7881 2020-02-10     6 recovered
    ## 1049  26.81540  106.8748 2020-02-10     3 recovered
    ## 1050  38.04280  114.5149 2020-02-10     7 recovered
    ## 1051  47.86200  127.7615 2020-02-10    16 recovered
    ## 1052  33.88202  113.6140 2020-02-10    38 recovered
    ## 1053  30.97560  112.2707 2020-02-10   427 recovered
    ## 1054  27.61040  111.7088 2020-02-10    22 recovered
    ## 1055  32.97110  119.4550 2020-02-10    10 recovered
    ## 1056  27.61400  115.7221 2020-02-10    32 recovered
    ## 1057  43.66610  126.1923 2020-02-10     1 recovered
    ## 1058  41.29560  122.6085 2020-02-10     1 recovered
    ## 1059  35.19170  108.8701 2020-02-10     5 recovered
    ## 1060  36.34270  118.1498 2020-02-10     3 recovered
    ## 1061  31.20200  121.4491 2020-02-10     4 recovered
    ## 1062  30.61710  102.7103 2020-02-10     9 recovered
    ## 1063  39.30540  117.3230 2020-02-10     4 recovered
    ## 1064  24.97400  101.4870 2020-02-10     1 recovered
    ## 1065  29.18320  120.0934 2020-02-10    41 recovered
    ## 1066  51.00000    9.0000 2020-02-11     2 confirmed
    ## 1067   1.28330  103.8333 2020-02-11     2 confirmed
    ## 1068  36.00000  128.0000 2020-02-11     1 confirmed
    ## 1069  15.00000  101.0000 2020-02-11     1 confirmed
    ## 1070  16.00000  108.0000 2020-02-11     1 confirmed
    ## 1071  31.82570  117.2264 2020-02-11    30 confirmed
    ## 1072  40.18240  116.4142 2020-02-11     5 confirmed
    ## 1073  30.05720  107.8740 2020-02-11    19 confirmed
    ## 1074  26.07890  117.9874 2020-02-11     6 confirmed
    ## 1075  36.06110  103.8343 2020-02-11     3 confirmed
    ## 1076  23.34170  113.4244 2020-02-11    18 confirmed
    ## 1077  23.82980  108.7881 2020-02-11     5 confirmed
    ## 1078  26.81540  106.8748 2020-02-11    18 confirmed
    ## 1079  19.19590  109.7453 2020-02-11     6 confirmed
    ## 1080  38.04280  114.5149 2020-02-11    21 confirmed
    ## 1081  47.86200  127.7615 2020-02-11    29 confirmed
    ## 1082  33.88202  113.6140 2020-02-11    32 confirmed
    ## 1083  22.30000  114.2000 2020-02-11    11 confirmed
    ## 1084  30.97560  112.2707 2020-02-11  1638 confirmed
    ## 1085  27.61040  111.7088 2020-02-11    33 confirmed
    ## 1086  32.97110  119.4550 2020-02-11    23 confirmed
    ## 1087  27.61400  115.7221 2020-02-11    33 confirmed
    ## 1088  43.66610  126.1923 2020-02-11     1 confirmed
    ## 1089  41.29560  122.6085 2020-02-11     3 confirmed
    ## 1090  37.26920  106.1655 2020-02-11     4 confirmed
    ## 1091  32.71570 -117.1611 2020-02-11     1 confirmed
    ## 1092  35.19170  108.8701 2020-02-11     6 confirmed
    ## 1093  36.34270  118.1498 2020-02-11    21 confirmed
    ## 1094  31.20200  121.4491 2020-02-11     4 confirmed
    ## 1095  37.57770  112.2922 2020-02-11     5 confirmed
    ## 1096  30.61710  102.7103 2020-02-11    12 confirmed
    ## 1097  39.30540  117.3230 2020-02-11    11 confirmed
    ## 1098  41.11290   85.2401 2020-02-11     6 confirmed
    ## 1099  24.97400  101.4870 2020-02-11     4 confirmed
    ## 1100  29.18320  120.0934 2020-02-11    25 confirmed
    ## 1101  31.82570  117.2264 2020-02-11     1     death
    ## 1102  40.18240  116.4142 2020-02-11     1     death
    ## 1103  30.05720  107.8740 2020-02-11     1     death
    ## 1104  47.86200  127.7615 2020-02-11     1     death
    ## 1105  33.88202  113.6140 2020-02-11     1     death
    ## 1106  30.97560  112.2707 2020-02-11    94     death
    ## 1107  39.30540  117.3230 2020-02-11     1     death
    ## 1108  36.00000  138.0000 2020-02-11     5 recovered
    ## 1109   2.50000  112.5000 2020-02-11     2 recovered
    ## 1110   1.28330  103.8333 2020-02-11     7 recovered
    ## 1111  16.00000  108.0000 2020-02-11     5 recovered
    ## 1112  31.82570  117.2264 2020-02-11    17 recovered
    ## 1113  40.18240  116.4142 2020-02-11     4 recovered
    ## 1114  30.05720  107.8740 2020-02-11    13 recovered
    ## 1115  26.07890  117.9874 2020-02-11     6 recovered
    ## 1116  36.06110  103.8343 2020-02-11     7 recovered
    ## 1117  23.34170  113.4244 2020-02-11    45 recovered
    ## 1118  23.82980  108.7881 2020-02-11     9 recovered
    ## 1119  26.81540  106.8748 2020-02-11     7 recovered
    ## 1120  19.19590  109.7453 2020-02-11     1 recovered
    ## 1121  38.04280  114.5149 2020-02-11     7 recovered
    ## 1122  47.86200  127.7615 2020-02-11    -2 recovered
    ## 1123  33.88202  113.6140 2020-02-11    27 recovered
    ## 1124  30.97560  112.2707 2020-02-11   417 recovered
    ## 1125  27.61040  111.7088 2020-02-11    39 recovered
    ## 1126  32.97110  119.4550 2020-02-11    12 recovered
    ## 1127  27.61400  115.7221 2020-02-11    23 recovered
    ## 1128  43.66610  126.1923 2020-02-11     5 recovered
    ## 1129  41.29560  122.6085 2020-02-11     6 recovered
    ## 1130  37.26920  106.1655 2020-02-11     9 recovered
    ## 1131  35.74520   95.9956 2020-02-11     2 recovered
    ## 1132  35.19170  108.8701 2020-02-11     2 recovered
    ## 1133  36.34270  118.1498 2020-02-11    14 recovered
    ## 1134  31.20200  121.4491 2020-02-11     4 recovered
    ## 1135  37.57770  112.2922 2020-02-11     5 recovered
    ## 1136  30.61710  102.7103 2020-02-11     5 recovered
    ## 1137  39.30540  117.3230 2020-02-11     2 recovered
    ## 1138  41.11290   85.2401 2020-02-11     3 recovered
    ## 1139  24.97400  101.4870 2020-02-11     1 recovered
    ## 1140  29.18320  120.0934 2020-02-11    28 recovered
    ## 1141  36.00000  138.0000 2020-02-12     2 confirmed
    ## 1142   1.28330  103.8333 2020-02-12     3 confirmed
    ## 1143  55.00000   -3.0000 2020-02-12     1 confirmed
    ## 1144  31.82570  117.2264 2020-02-12    29 confirmed
    ## 1145  40.18240  116.4142 2020-02-12    10 confirmed
    ## 1146  30.05720  107.8740 2020-02-12    13 confirmed
    ## 1147  35.44370  139.6380 2020-02-12    40 confirmed
    ## 1148  26.07890  117.9874 2020-02-12     5 confirmed
    ## 1149  36.06110  103.8343 2020-02-12     1 confirmed
    ## 1150  23.34170  113.4244 2020-02-12    42 confirmed
    ## 1151  23.82980  108.7881 2020-02-12     7 confirmed
    ## 1152  26.81540  106.8748 2020-02-12     6 confirmed
    ## 1153  19.19590  109.7453 2020-02-12    13 confirmed
    ## 1154  38.04280  114.5149 2020-02-12    12 confirmed
    ## 1155  47.86200  127.7615 2020-02-12    18 confirmed
    ## 1156  33.88202  113.6140 2020-02-12    30 confirmed
    ## 1157  22.30000  114.2000 2020-02-12     1 confirmed
    ## 1158  27.61040  111.7088 2020-02-12    34 confirmed
    ## 1159  44.09350  113.9448 2020-02-12     2 confirmed
    ## 1160  32.97110  119.4550 2020-02-12    28 confirmed
    ## 1161  27.61400  115.7221 2020-02-12    40 confirmed
    ## 1162  43.66610  126.1923 2020-02-12     2 confirmed
    ## 1163  41.29560  122.6085 2020-02-12     5 confirmed
    ## 1164  37.26920  106.1655 2020-02-12     5 confirmed
    ## 1165  35.19170  108.8701 2020-02-12     6 confirmed
    ## 1166  36.34270  118.1498 2020-02-12    10 confirmed
    ## 1167  31.20200  121.4491 2020-02-12     8 confirmed
    ## 1168  37.57770  112.2922 2020-02-12     2 confirmed
    ## 1169  30.61710  102.7103 2020-02-12    19 confirmed
    ## 1170  39.30540  117.3230 2020-02-12     6 confirmed
    ## 1171  41.11290   85.2401 2020-02-12     4 confirmed
    ## 1172  24.97400  101.4870 2020-02-12     1 confirmed
    ## 1173  29.18320  120.0934 2020-02-12    14 confirmed
    ## 1174  19.19590  109.7453 2020-02-12     1     death
    ## 1175  33.88202  113.6140 2020-02-12     1     death
    ## 1176  27.61040  111.7088 2020-02-12     1     death
    ## 1177  41.29560  122.6085 2020-02-12     1     death
    ## 1178  36.34270  118.1498 2020-02-12     1     death
    ## 1179  11.55000  104.9167 2020-02-12     1 recovered
    ## 1180  64.00000   26.0000 2020-02-12     1 recovered
    ## 1181  47.00000    2.0000 2020-02-12     2 recovered
    ## 1182  28.16670   84.2500 2020-02-12     1 recovered
    ## 1183  13.00000  122.0000 2020-02-12     1 recovered
    ## 1184  60.00000   90.0000 2020-02-12     2 recovered
    ## 1185   1.28330  103.8333 2020-02-12     6 recovered
    ## 1186  36.00000  128.0000 2020-02-12     4 recovered
    ## 1187  55.00000   -3.0000 2020-02-12     1 recovered
    ## 1188  24.00000   54.0000 2020-02-12     1 recovered
    ## 1189  31.82570  117.2264 2020-02-12    22 recovered
    ## 1190  40.18240  116.4142 2020-02-12     8 recovered
    ## 1191  30.05720  107.8740 2020-02-12    23 recovered
    ## 1192  26.07890  117.9874 2020-02-12     8 recovered
    ## 1193  36.06110  103.8343 2020-02-12     7 recovered
    ## 1194  23.34170  113.4244 2020-02-12    63 recovered
    ## 1195  23.82980  108.7881 2020-02-12    -1 recovered
    ## 1196  26.81540  106.8748 2020-02-12     1 recovered
    ## 1197  19.19590  109.7453 2020-02-12     7 recovered
    ## 1198  38.04280  114.5149 2020-02-12     6 recovered
    ## 1199  47.86200  127.7615 2020-02-12     3 recovered
    ## 1200  33.88202  113.6140 2020-02-12    28 recovered
    ## 1201  22.30000  114.2000 2020-02-12     1 recovered
    ## 1202  30.97560  112.2707 2020-02-12    47 recovered
    ## 1203  27.61040  111.7088 2020-02-12    57 recovered
    ## 1204  44.09350  113.9448 2020-02-12     1 recovered
    ## 1205  32.97110  119.4550 2020-02-12    32 recovered
    ## 1206  27.61400  115.7221 2020-02-12    24 recovered
    ## 1207  43.66610  126.1923 2020-02-12     4 recovered
    ## 1208  41.29560  122.6085 2020-02-12     1 recovered
    ## 1209  42.98490  -81.2453 2020-02-12     1 recovered
    ## 1210  22.16670  113.5500 2020-02-12     1 recovered
    ## 1211  37.26920  106.1655 2020-02-12     2 recovered
    ## 1212  35.74520   95.9956 2020-02-12     4 recovered
    ## 1213  35.19170  108.8701 2020-02-12    11 recovered
    ## 1214  36.34270  118.1498 2020-02-12    12 recovered
    ## 1215  31.20200  121.4491 2020-02-12     5 recovered
    ## 1216  37.57770  112.2922 2020-02-12     3 recovered
    ## 1217  30.61710  102.7103 2020-02-12     7 recovered
    ## 1218  39.30540  117.3230 2020-02-12     1 recovered
    ## 1219  31.69270   88.0924 2020-02-12     1 recovered
    ## 1220  24.97400  101.4870 2020-02-12     6 recovered
    ## 1221  29.18320  120.0934 2020-02-12    51 recovered
    ## 1222   2.50000  112.5000 2020-02-13     1 confirmed
    ## 1223   1.28330  103.8333 2020-02-13     8 confirmed
    ## 1224  16.00000  108.0000 2020-02-13     1 confirmed
    ## 1225  31.82570  117.2264 2020-02-13    21 confirmed
    ## 1226  40.18240  116.4142 2020-02-13    14 confirmed
    ## 1227  30.05720  107.8740 2020-02-13    11 confirmed
    ## 1228  26.07890  117.9874 2020-02-13     7 confirmed
    ## 1229  36.06110  103.8343 2020-02-13     3 confirmed
    ## 1230  23.34170  113.4244 2020-02-13    22 confirmed
    ## 1231  26.81540  106.8748 2020-02-13     2 confirmed
    ## 1232  38.04280  114.5149 2020-02-13    14 confirmed
    ## 1233  47.86200  127.7615 2020-02-13    17 confirmed
    ## 1234  33.88202  113.6140 2020-02-13    34 confirmed
    ## 1235  22.30000  114.2000 2020-02-13     3 confirmed
    ## 1236  30.97560  112.2707 2020-02-13 14840 confirmed
    ## 1237  27.61040  111.7088 2020-02-13    22 confirmed
    ## 1238  44.09350  113.9448 2020-02-13     1 confirmed
    ## 1239  32.97110  119.4550 2020-02-13    27 confirmed
    ## 1240  27.61400  115.7221 2020-02-13    28 confirmed
    ## 1241  43.66610  126.1923 2020-02-13     1 confirmed
    ## 1242  41.29560  122.6085 2020-02-13     1 confirmed
    ## 1243  37.26920  106.1655 2020-02-13     6 confirmed
    ## 1244  29.42410  -98.4936 2020-02-13     1 confirmed
    ## 1245  32.71570 -117.1611 2020-02-13     1 confirmed
    ## 1246  35.19170  108.8701 2020-02-13     4 confirmed
    ## 1247  36.34270  118.1498 2020-02-13    12 confirmed
    ## 1248  31.20200  121.4491 2020-02-13     4 confirmed
    ## 1249  30.61710  102.7103 2020-02-13    15 confirmed
    ## 1250  39.30540  117.3230 2020-02-13     7 confirmed
    ## 1251  41.11290   85.2401 2020-02-13     4 confirmed
    ## 1252  24.97400  101.4870 2020-02-13     2 confirmed
    ## 1253  29.18320  120.0934 2020-02-13    14 confirmed
    ## 1254  36.00000  138.0000 2020-02-13     1     death
    ## 1255  31.82570  117.2264 2020-02-13     1     death
    ## 1256  30.05720  107.8740 2020-02-13     1     death
    ## 1257  23.34170  113.4244 2020-02-13     1     death
    ## 1258  23.82980  108.7881 2020-02-13     1     death
    ## 1259  38.04280  114.5149 2020-02-13     1     death
    ## 1260  47.86200  127.7615 2020-02-13     1     death
    ## 1261  33.88202  113.6140 2020-02-13     2     death
    ## 1262  30.97560  112.2707 2020-02-13   242     death
    ## 1263  39.30540  117.3230 2020-02-13     1     death
    ## 1264  41.11290   85.2401 2020-02-13     1     death
    ## 1265  51.00000    9.0000 2020-02-13     1 recovered
    ## 1266  15.00000  101.0000 2020-02-13     2 recovered
    ## 1267  16.00000  108.0000 2020-02-13     1 recovered
    ## 1268  31.82570  117.2264 2020-02-13    30 recovered
    ## 1269  40.18240  116.4142 2020-02-13    13 recovered
    ## 1270  30.05720  107.8740 2020-02-13    26 recovered
    ## 1271  26.07890  117.9874 2020-02-13     4 recovered
    ## 1272  36.06110  103.8343 2020-02-13     8 recovered
    ## 1273  23.34170  113.4244 2020-02-13    39 recovered
    ## 1274  23.82980  108.7881 2020-02-13     1 recovered
    ## 1275  26.81540  106.8748 2020-02-13     9 recovered
    ## 1276  19.19590  109.7453 2020-02-13     3 recovered
    ## 1277  38.04280  114.5149 2020-02-13    14 recovered
    ## 1278  47.86200  127.7615 2020-02-13     2 recovered
    ## 1279  33.88202  113.6140 2020-02-13    50 recovered
    ## 1280  30.97560  112.2707 2020-02-13   773 recovered
    ## 1281  27.61040  111.7088 2020-02-13    35 recovered
    ## 1282  32.97110  119.4550 2020-02-13    14 recovered
    ## 1283  27.61400  115.7221 2020-02-13    18 recovered
    ## 1284  43.66610  126.1923 2020-02-13     2 recovered
    ## 1285  41.29560  122.6085 2020-02-13     2 recovered
    ## 1286  22.16670  113.5500 2020-02-13     1 recovered
    ## 1287 -33.86880  151.2093 2020-02-13     2 recovered
    ## 1288  35.74520   95.9956 2020-02-13     2 recovered
    ## 1289  35.19170  108.8701 2020-02-13     3 recovered
    ## 1290  36.34270  118.1498 2020-02-13    13 recovered
    ## 1291  31.20200  121.4491 2020-02-13     5 recovered
    ## 1292  37.57770  112.2922 2020-02-13     3 recovered
    ## 1293  30.61710  102.7103 2020-02-13    12 recovered
    ## 1294  39.30540  117.3230 2020-02-13    10 recovered
    ## 1295 -37.81360  144.9631 2020-02-13     4 recovered
    ## 1296  41.11290   85.2401 2020-02-13     3 recovered
    ## 1297  24.97400  101.4870 2020-02-13     1 recovered
    ## 1298  29.18320  120.0934 2020-02-13    39 recovered
    ## 1299  26.00000   30.0000 2020-02-14     1 confirmed
    ## 1300  36.00000  138.0000 2020-02-14     1 confirmed
    ## 1301   1.28330  103.8333 2020-02-14     9 confirmed
    ## 1302  31.82570  117.2264 2020-02-14    24 confirmed
    ## 1303  40.18240  116.4142 2020-02-14     6 confirmed
    ## 1304  30.05720  107.8740 2020-02-14     8 confirmed
    ## 1305  35.44370  139.6380 2020-02-14    43 confirmed
    ## 1306  26.07890  117.9874 2020-02-14     2 confirmed
    ## 1307  23.34170  113.4244 2020-02-14    20 confirmed
    ## 1308  23.82980  108.7881 2020-02-14     4 confirmed
    ## 1309  26.81540  106.8748 2020-02-14     5 confirmed
    ## 1310  19.19590  109.7453 2020-02-14     2 confirmed
    ## 1311  38.04280  114.5149 2020-02-14    18 confirmed
    ## 1312  47.86200  127.7615 2020-02-14    24 confirmed
    ## 1313  33.88202  113.6140 2020-02-14    15 confirmed
    ## 1314  22.30000  114.2000 2020-02-14     3 confirmed
    ## 1315  30.97560  112.2707 2020-02-14  6200 confirmed
    ## 1316  27.61040  111.7088 2020-02-14    20 confirmed
    ## 1317  44.09350  113.9448 2020-02-14     4 confirmed
    ## 1318  32.97110  119.4550 2020-02-14    23 confirmed
    ## 1319  27.61400  115.7221 2020-02-14    28 confirmed
    ## 1320  43.66610  126.1923 2020-02-14     2 confirmed
    ## 1321  41.29560  122.6085 2020-02-14     2 confirmed
    ## 1322  37.26920  106.1655 2020-02-14     3 confirmed
    ## 1323  35.19170  108.8701 2020-02-14     1 confirmed
    ## 1324  36.34270  118.1498 2020-02-14    14 confirmed
    ## 1325  31.20200  121.4491 2020-02-14     3 confirmed
    ## 1326  37.57770  112.2922 2020-02-14     1 confirmed
    ## 1327  30.61710  102.7103 2020-02-14    12 confirmed
    ## 1328  39.30540  117.3230 2020-02-14     1 confirmed
    ## 1329  41.11290   85.2401 2020-02-14     2 confirmed
    ## 1330  24.97400  101.4870 2020-02-14     6 confirmed
    ## 1331  29.18320  120.0934 2020-02-14    10 confirmed
    ## 1332  31.82570  117.2264 2020-02-14     1     death
    ## 1333  30.05720  107.8740 2020-02-14     1     death
    ## 1334  47.86200  127.7615 2020-02-14     2     death
    ## 1335  33.88202  113.6140 2020-02-14     1     death
    ## 1336  30.97560  112.2707 2020-02-14   147     death
    ## 1337   1.28330  103.8333 2020-02-14     2 recovered
    ## 1338  31.82570  117.2264 2020-02-14    36 recovered
    ## 1339  40.18240  116.4142 2020-02-14    11 recovered
    ## 1340  30.05720  107.8740 2020-02-14    24 recovered
    ## 1341  26.07890  117.9874 2020-02-14     6 recovered
    ## 1342  23.34170  113.4244 2020-02-14    48 recovered
    ## 1343  23.82980  108.7881 2020-02-14     3 recovered
    ## 1344  26.81540  106.8748 2020-02-14     1 recovered
    ## 1345  19.19590  109.7453 2020-02-14    13 recovered
    ## 1346  38.04280  114.5149 2020-02-14    19 recovered
    ## 1347  47.86200  127.7615 2020-02-14    14 recovered
    ## 1348  33.88202  113.6140 2020-02-14    61 recovered
    ## 1349  30.97560  112.2707 2020-02-14  1315 recovered
    ## 1350  27.61040  111.7088 2020-02-14    25 recovered
    ## 1351  32.97110  119.4550 2020-02-14    18 recovered
    ## 1352  27.61400  115.7221 2020-02-14    17 recovered
    ## 1353  43.66610  126.1923 2020-02-14     1 recovered
    ## 1354  41.29560  122.6085 2020-02-14     7 recovered
    ## 1355  35.19170  108.8701 2020-02-14     8 recovered
    ## 1356  36.34270  118.1498 2020-02-14    31 recovered
    ## 1357  31.20200  121.4491 2020-02-14    28 recovered
    ## 1358  37.57770  112.2922 2020-02-14     2 recovered
    ## 1359  30.61710  102.7103 2020-02-14    10 recovered
    ## 1360  23.70000  121.0000 2020-02-14     1 recovered
    ## 1361  39.30540  117.3230 2020-02-14    10 recovered
    ## 1362  24.97400  101.4870 2020-02-14     9 recovered
    ## 1363  29.18320  120.0934 2020-02-14    43 recovered
    ## 1364  47.00000    2.0000 2020-02-15     1 confirmed
    ## 1365  36.00000  138.0000 2020-02-15    14 confirmed
    ## 1366   2.50000  112.5000 2020-02-15     3 confirmed
    ## 1367   1.28330  103.8333 2020-02-15     5 confirmed
    ## 1368  31.82570  117.2264 2020-02-15    16 confirmed
    ## 1369  40.18240  116.4142 2020-02-15     3 confirmed
    ## 1370  30.05720  107.8740 2020-02-15     7 confirmed
    ## 1371  35.44370  139.6380 2020-02-15    67 confirmed
    ## 1372  26.07890  117.9874 2020-02-15     4 confirmed
    ## 1373  23.34170  113.4244 2020-02-15    33 confirmed
    ## 1374  23.82980  108.7881 2020-02-15     9 confirmed
    ## 1375  26.81540  106.8748 2020-02-15     3 confirmed
    ## 1376  19.19590  109.7453 2020-02-15     3 confirmed
    ## 1377  38.04280  114.5149 2020-02-15     8 confirmed
    ## 1378  47.86200  127.7615 2020-02-15     6 confirmed
    ## 1379  33.88202  113.6140 2020-02-15    28 confirmed
    ## 1380  30.97560  112.2707 2020-02-15  1843 confirmed
    ## 1381  27.61040  111.7088 2020-02-15    13 confirmed
    ## 1382  44.09350  113.9448 2020-02-15     3 confirmed
    ## 1383  32.97110  119.4550 2020-02-15    11 confirmed
    ## 1384  27.61400  115.7221 2020-02-15    13 confirmed
    ## 1385  43.66610  126.1923 2020-02-15     2 confirmed
    ## 1386  37.26920  106.1655 2020-02-15     3 confirmed
    ## 1387  35.19170  108.8701 2020-02-15     2 confirmed
    ## 1388  36.34270  118.1498 2020-02-15     9 confirmed
    ## 1389  31.20200  121.4491 2020-02-15     8 confirmed
    ## 1390  37.57770  112.2922 2020-02-15     1 confirmed
    ## 1391  30.61710  102.7103 2020-02-15     7 confirmed
    ## 1392  39.30540  117.3230 2020-02-15     2 confirmed
    ## 1393  41.11290   85.2401 2020-02-15     5 confirmed
    ## 1394  24.97400  101.4870 2020-02-15     6 confirmed
    ## 1395  29.18320  120.0934 2020-02-15     7 confirmed
    ## 1396  47.00000    2.0000 2020-02-15     1     death
    ## 1397  40.18240  116.4142 2020-02-15     1     death
    ## 1398  33.88202  113.6140 2020-02-15     2     death
    ## 1399  30.97560  112.2707 2020-02-15   139     death
    ## 1400  47.00000    2.0000 2020-02-15     2 recovered
    ## 1401  36.00000  138.0000 2020-02-15     3 recovered
    ## 1402   2.50000  112.5000 2020-02-15     4 recovered
    ## 1403   1.28330  103.8333 2020-02-15     1 recovered
    ## 1404  36.00000  128.0000 2020-02-15     2 recovered
    ## 1405  40.00000   -4.0000 2020-02-15     2 recovered
    ## 1406  24.00000   54.0000 2020-02-15     2 recovered
    ## 1407  31.82570  117.2264 2020-02-15    28 recovered
    ## 1408  40.18240  116.4142 2020-02-15    18 recovered
    ## 1409  30.05720  107.8740 2020-02-15    32 recovered
    ## 1410  26.07890  117.9874 2020-02-15     8 recovered
    ## 1411  36.06110  103.8343 2020-02-15    10 recovered
    ## 1412  23.34170  113.4244 2020-02-15    48 recovered
    ## 1413  23.82980  108.7881 2020-02-15     8 recovered
    ## 1414  26.81540  106.8748 2020-02-15    13 recovered
    ## 1415  19.19590  109.7453 2020-02-15    -4 recovered
    ## 1416  38.04280  114.5149 2020-02-15    14 recovered
    ## 1417  47.86200  127.7615 2020-02-15    21 recovered
    ## 1418  33.88202  113.6140 2020-02-15    34 recovered
    ## 1419  30.97560  112.2707 2020-02-15   849 recovered
    ## 1420  27.61040  111.7088 2020-02-15    61 recovered
    ## 1421  44.09350  113.9448 2020-02-15     1 recovered
    ## 1422  32.97110  119.4550 2020-02-15    29 recovered
    ## 1423  27.61400  115.7221 2020-02-15    23 recovered
    ## 1424  43.66610  126.1923 2020-02-15     1 recovered
    ## 1425  41.29560  122.6085 2020-02-15     2 recovered
    ## 1426  37.26920  106.1655 2020-02-15     9 recovered
    ## 1427  35.74520   95.9956 2020-02-15     2 recovered
    ## 1428  35.19170  108.8701 2020-02-15     6 recovered
    ## 1429  36.34270  118.1498 2020-02-15    20 recovered
    ## 1430  31.20200  121.4491 2020-02-15    34 recovered
    ## 1431  37.57770  112.2922 2020-02-15     8 recovered
    ## 1432  30.61710  102.7103 2020-02-15     5 recovered
    ## 1433  39.30540  117.3230 2020-02-15     6 recovered
    ## 1434  41.11290   85.2401 2020-02-15     4 recovered
    ## 1435  24.97400  101.4870 2020-02-15     6 recovered
    ## 1436  29.18320  120.0934 2020-02-15    25 recovered
    ## 1437  36.00000  138.0000 2020-02-16    16 confirmed
    ## 1438   1.28330  103.8333 2020-02-16     3 confirmed
    ## 1439  36.00000  128.0000 2020-02-16     1 confirmed
    ## 1440  15.00000  101.0000 2020-02-16     1 confirmed
    ## 1441  24.00000   54.0000 2020-02-16     1 confirmed
    ## 1442  31.82570  117.2264 2020-02-16    12 confirmed
    ## 1443  40.18240  116.4142 2020-02-16     5 confirmed
    ## 1444  30.05720  107.8740 2020-02-16     7 confirmed
    ## 1445  35.44370  139.6380 2020-02-16    70 confirmed
    ## 1446  26.07890  117.9874 2020-02-16     2 confirmed
    ## 1447  23.34170  113.4244 2020-02-16    22 confirmed
    ## 1448  23.82980  108.7881 2020-02-16     2 confirmed
    ## 1449  26.81540  106.8748 2020-02-16     1 confirmed
    ## 1450  38.04280  114.5149 2020-02-16     9 confirmed
    ## 1451  47.86200  127.7615 2020-02-16    20 confirmed
    ## 1452  33.88202  113.6140 2020-02-16    19 confirmed
    ## 1453  22.30000  114.2000 2020-02-16     1 confirmed
    ## 1454  30.97560  112.2707 2020-02-16  1933 confirmed
    ## 1455  27.61040  111.7088 2020-02-16     3 confirmed
    ## 1456  44.09350  113.9448 2020-02-16     2 confirmed
    ## 1457  32.97110  119.4550 2020-02-16    13 confirmed
    ## 1458  27.61400  115.7221 2020-02-16    12 confirmed
    ## 1459  43.66610  126.1923 2020-02-16     1 confirmed
    ## 1460  41.29560  122.6085 2020-02-16     2 confirmed
    ## 1461  35.19170  108.8701 2020-02-16     4 confirmed
    ## 1462  36.34270  118.1498 2020-02-16     5 confirmed
    ## 1463  31.20200  121.4491 2020-02-16     2 confirmed
    ## 1464  37.57770  112.2922 2020-02-16     1 confirmed
    ## 1465  30.61710  102.7103 2020-02-16    11 confirmed
    ## 1466  23.70000  121.0000 2020-02-16     2 confirmed
    ## 1467  39.30540  117.3230 2020-02-16     2 confirmed
    ## 1468  41.11290   85.2401 2020-02-16     1 confirmed
    ## 1469  24.97400  101.4870 2020-02-16     3 confirmed
    ## 1470  29.18320  120.0934 2020-02-16     5 confirmed
    ## 1471  30.97560  112.2707 2020-02-16   100     death
    ## 1472  27.61040  111.7088 2020-02-16     1     death
    ## 1473  30.61710  102.7103 2020-02-16     2     death
    ## 1474  23.70000  121.0000 2020-02-16     1     death
    ## 1475  21.00000   78.0000 2020-02-16     3 recovered
    ## 1476  15.00000  101.0000 2020-02-16     2 recovered
    ## 1477  55.00000   -3.0000 2020-02-16     7 recovered
    ## 1478  24.00000   54.0000 2020-02-16     1 recovered
    ## 1479  31.82570  117.2264 2020-02-16    34 recovered
    ## 1480  40.18240  116.4142 2020-02-16    10 recovered
    ## 1481  30.05720  107.8740 2020-02-16    23 recovered
    ## 1482  26.07890  117.9874 2020-02-16    11 recovered
    ## 1483  36.06110  103.8343 2020-02-16     5 recovered
    ## 1484  23.34170  113.4244 2020-02-16    55 recovered
    ## 1485  23.82980  108.7881 2020-02-16     5 recovered
    ## 1486  26.81540  106.8748 2020-02-16     5 recovered
    ## 1487  19.19590  109.7453 2020-02-16    13 recovered
    ## 1488  38.04280  114.5149 2020-02-16     4 recovered
    ## 1489  47.86200  127.7615 2020-02-16    11 recovered
    ## 1490  33.88202  113.6140 2020-02-16    49 recovered
    ## 1491  22.30000  114.2000 2020-02-16     1 recovered
    ## 1492  30.97560  112.2707 2020-02-16  1016 recovered
    ## 1493  27.61040  111.7088 2020-02-16    39 recovered
    ## 1494  44.09350  113.9448 2020-02-16     1 recovered
    ## 1495  32.97110  119.4550 2020-02-16    32 recovered
    ## 1496  27.61400  115.7221 2020-02-16    30 recovered
    ## 1497  43.66610  126.1923 2020-02-16     4 recovered
    ## 1498  41.29560  122.6085 2020-02-16     9 recovered
    ## 1499  22.16670  113.5500 2020-02-16     2 recovered
    ## 1500  35.19170  108.8701 2020-02-16    11 recovered
    ## 1501  36.34270  118.1498 2020-02-16    17 recovered
    ## 1502  31.20200  121.4491 2020-02-16    16 recovered
    ## 1503  37.57770  112.2922 2020-02-16     4 recovered
    ## 1504  30.61710  102.7103 2020-02-16    12 recovered
    ## 1505  39.30540  117.3230 2020-02-16     8 recovered
    ## 1506  41.11290   85.2401 2020-02-16     2 recovered
    ## 1507  29.18320  120.0934 2020-02-16    28 recovered
    ## 1508  36.00000  138.0000 2020-02-17     7 confirmed
    ## 1509   1.28330  103.8333 2020-02-17     2 confirmed
    ## 1510  36.00000  128.0000 2020-02-17     1 confirmed
    ## 1511  15.00000  101.0000 2020-02-17     1 confirmed
    ## 1512  31.82570  117.2264 2020-02-17    11 confirmed
    ## 1513  40.18240  116.4142 2020-02-17     1 confirmed
    ## 1514  49.28270 -123.1207 2020-02-17     1 confirmed
    ## 1515  30.05720  107.8740 2020-02-17     2 confirmed
    ## 1516  35.44370  139.6380 2020-02-17    99 confirmed
    ## 1517  26.07890  117.9874 2020-02-17     3 confirmed
    ## 1518  36.06110  103.8343 2020-02-17     1 confirmed
    ## 1519  23.34170  113.4244 2020-02-17     6 confirmed
    ## 1520  23.82980  108.7881 2020-02-17     1 confirmed
    ## 1521  26.81540  106.8748 2020-02-17     2 confirmed
    ## 1522  19.19590  109.7453 2020-02-17     1 confirmed
    ## 1523  38.04280  114.5149 2020-02-17     1 confirmed
    ## 1524  47.86200  127.7615 2020-02-17    12 confirmed
    ## 1525  33.88202  113.6140 2020-02-17    15 confirmed
    ## 1526  22.30000  114.2000 2020-02-17     3 confirmed
    ## 1527  30.97560  112.2707 2020-02-17  1807 confirmed
    ## 1528  27.61040  111.7088 2020-02-17     2 confirmed
    ## 1529  44.09350  113.9448 2020-02-17     2 confirmed
    ## 1530  32.97110  119.4550 2020-02-17     9 confirmed
    ## 1531  27.61400  115.7221 2020-02-17     5 confirmed
    ## 1532  35.19170  108.8701 2020-02-17     4 confirmed
    ## 1533  36.34270  118.1498 2020-02-17     4 confirmed
    ## 1534  31.20200  121.4491 2020-02-17     5 confirmed
    ## 1535  37.57770  112.2922 2020-02-17     1 confirmed
    ## 1536  30.61710  102.7103 2020-02-17    14 confirmed
    ## 1537  23.70000  121.0000 2020-02-17     2 confirmed
    ## 1538  39.30540  117.3230 2020-02-17     1 confirmed
    ## 1539  41.11290   85.2401 2020-02-17     4 confirmed
    ## 1540  29.18320  120.0934 2020-02-17     4 confirmed
    ## 1541  23.34170  113.4244 2020-02-17     2     death
    ## 1542  33.88202  113.6140 2020-02-17     3     death
    ## 1543  30.97560  112.2707 2020-02-17    93     death
    ## 1544  50.83330    4.0000 2020-02-17     1 recovered
    ## 1545   1.28330  103.8333 2020-02-17     6 recovered
    ## 1546  36.00000  128.0000 2020-02-17     1 recovered
    ## 1547  15.00000  101.0000 2020-02-17     1 recovered
    ## 1548  31.82570  117.2264 2020-02-17    25 recovered
    ## 1549  40.18240  116.4142 2020-02-17     6 recovered
    ## 1550  30.05720  107.8740 2020-02-17    18 recovered
    ## 1551  26.07890  117.9874 2020-02-17     8 recovered
    ## 1552  36.06110  103.8343 2020-02-17     4 recovered
    ## 1553  23.34170  113.4244 2020-02-17    59 recovered
    ## 1554  23.82980  108.7881 2020-02-17     4 recovered
    ## 1555  26.81540  106.8748 2020-02-17    11 recovered
    ## 1556  19.19590  109.7453 2020-02-17     7 recovered
    ## 1557  38.04280  114.5149 2020-02-17    17 recovered
    ## 1558  47.86200  127.7615 2020-02-17     6 recovered
    ## 1559  33.88202  113.6140 2020-02-17    69 recovered
    ## 1560  30.97560  112.2707 2020-02-17  1223 recovered
    ## 1561  27.61040  111.7088 2020-02-17    34 recovered
    ## 1562  32.97110  119.4550 2020-02-17    40 recovered
    ## 1563  27.61400  115.7221 2020-02-17    35 recovered
    ## 1564  43.66610  126.1923 2020-02-17     4 recovered
    ## 1565  41.29560  122.6085 2020-02-17     3 recovered
    ## 1566  37.26920  106.1655 2020-02-17     2 recovered
    ## 1567  35.19170  108.8701 2020-02-17     8 recovered
    ## 1568  36.34270  118.1498 2020-02-17    18 recovered
    ## 1569  31.20200  121.4491 2020-02-17    21 recovered
    ## 1570  37.57770  112.2922 2020-02-17     3 recovered
    ## 1571  30.61710  102.7103 2020-02-17    25 recovered
    ## 1572 -34.92850  138.6007 2020-02-17     2 recovered
    ## 1573  39.30540  117.3230 2020-02-17     1 recovered
    ## 1574  24.97400  101.4870 2020-02-17     5 recovered
    ## 1575  29.18320  120.0934 2020-02-17    51 recovered
    ## 1576  36.00000  138.0000 2020-02-18     8 confirmed
    ## 1577   1.28330  103.8333 2020-02-18     4 confirmed
    ## 1578  36.00000  128.0000 2020-02-18     1 confirmed
    ## 1579  31.82570  117.2264 2020-02-18     9 confirmed
    ## 1580  40.18240  116.4142 2020-02-18     6 confirmed
    ## 1581  30.05720  107.8740 2020-02-18     2 confirmed
    ## 1582  35.44370  139.6380 2020-02-18    88 confirmed
    ## 1583  26.07890  117.9874 2020-02-18     2 confirmed
    ## 1584  23.34170  113.4244 2020-02-18     6 confirmed
    ## 1585  23.82980  108.7881 2020-02-18     4 confirmed
    ## 1586  38.04280  114.5149 2020-02-18     5 confirmed
    ## 1587  47.86200  127.7615 2020-02-18     7 confirmed
    ## 1588  33.88202  113.6140 2020-02-18    11 confirmed
    ## 1589  22.30000  114.2000 2020-02-18     2 confirmed
    ## 1590  30.97560  112.2707 2020-02-18  1693 confirmed
    ## 1591  27.61040  111.7088 2020-02-18     1 confirmed
    ## 1592  44.09350  113.9448 2020-02-18     1 confirmed
    ## 1593  32.97110  119.4550 2020-02-18     3 confirmed
    ## 1594  27.61400  115.7221 2020-02-18     3 confirmed
    ## 1595  36.34270  118.1498 2020-02-18     2 confirmed
    ## 1596  37.57770  112.2922 2020-02-18     1 confirmed
    ## 1597  30.61710  102.7103 2020-02-18    13 confirmed
    ## 1598  39.30540  117.3230 2020-02-18     3 confirmed
    ## 1599  41.11290   85.2401 2020-02-18     1 confirmed
    ## 1600  24.97400  101.4870 2020-02-18     1 confirmed
    ## 1601  29.18320  120.0934 2020-02-18     1 confirmed
    ## 1602  26.81540  106.8748 2020-02-18     1     death
    ## 1603  38.04280  114.5149 2020-02-18     1     death
    ## 1604  33.88202  113.6140 2020-02-18     3     death
    ## 1605  30.97560  112.2707 2020-02-18   132     death
    ## 1606  27.61040  111.7088 2020-02-18     1     death
    ## 1607  36.34270  118.1498 2020-02-18     1     death
    ## 1608  51.00000    9.0000 2020-02-18    11 recovered
    ## 1609  36.00000  138.0000 2020-02-18     1 recovered
    ## 1610   2.50000  112.5000 2020-02-18     6 recovered
    ## 1611   1.28330  103.8333 2020-02-18     5 recovered
    ## 1612  36.00000  128.0000 2020-02-18     2 recovered
    ## 1613  31.82570  117.2264 2020-02-18    81 recovered
    ## 1614  40.18240  116.4142 2020-02-18     8 recovered
    ## 1615  30.05720  107.8740 2020-02-18    29 recovered
    ## 1616  26.07890  117.9874 2020-02-18     3 recovered
    ## 1617  36.06110  103.8343 2020-02-18     4 recovered
    ## 1618  23.34170  113.4244 2020-02-18    41 recovered
    ## 1619  23.82980  108.7881 2020-02-18    16 recovered
    ## 1620  26.81540  106.8748 2020-02-18     9 recovered
    ## 1621  19.19590  109.7453 2020-02-18    20 recovered
    ## 1622  38.04280  114.5149 2020-02-18    14 recovered
    ## 1623  47.86200  127.7615 2020-02-18    26 recovered
    ## 1624  33.88202  113.6140 2020-02-18    13 recovered
    ## 1625  30.97560  112.2707 2020-02-18  1266 recovered
    ## 1626  27.61040  111.7088 2020-02-18    29 recovered
    ## 1627  32.97110  119.4550 2020-02-18    22 recovered
    ## 1628  27.61400  115.7221 2020-02-18    35 recovered
    ## 1629  43.66610  126.1923 2020-02-18     2 recovered
    ## 1630  41.29560  122.6085 2020-02-18    10 recovered
    ## 1631  37.26920  106.1655 2020-02-18     7 recovered
    ## 1632  35.74520   95.9956 2020-02-18     2 recovered
    ## 1633  35.19170  108.8701 2020-02-18    10 recovered
    ## 1634  36.34270  118.1498 2020-02-18    20 recovered
    ## 1635  31.20200  121.4491 2020-02-18    16 recovered
    ## 1636  37.57770  112.2922 2020-02-18     8 recovered
    ## 1637  30.61710  102.7103 2020-02-18    13 recovered
    ## 1638  39.30540  117.3230 2020-02-18     2 recovered
    ## 1639  24.97400  101.4870 2020-02-18    10 recovered
    ## 1640  29.18320  120.0934 2020-02-18    28 recovered
    ## 1641  32.00000   53.0000 2020-02-19     2 confirmed
    ## 1642  36.00000  138.0000 2020-02-19    10 confirmed
    ## 1643   1.28330  103.8333 2020-02-19     3 confirmed
    ## 1644  31.82570  117.2264 2020-02-19     4 confirmed
    ## 1645  40.18240  116.4142 2020-02-19     6 confirmed
    ## 1646  30.05720  107.8740 2020-02-19     5 confirmed
    ## 1647  35.44370  139.6380 2020-02-19    79 confirmed
    ## 1648  26.07890  117.9874 2020-02-19     1 confirmed
    ## 1649  23.34170  113.4244 2020-02-19     3 confirmed
    ## 1650  23.82980  108.7881 2020-02-19     2 confirmed
    ## 1651  19.19590  109.7453 2020-02-19     5 confirmed
    ## 1652  47.86200  127.7615 2020-02-19     6 confirmed
    ## 1653  33.88202  113.6140 2020-02-19     5 confirmed
    ## 1654  22.30000  114.2000 2020-02-19     1 confirmed
    ## 1655  30.97560  112.2707 2020-02-19   349 confirmed
    ## 1656  27.61040  111.7088 2020-02-19     1 confirmed
    ## 1657  44.09350  113.9448 2020-02-19     2 confirmed
    ## 1658  32.97110  119.4550 2020-02-19     2 confirmed
    ## 1659  27.61400  115.7221 2020-02-19     1 confirmed
    ## 1660  43.66610  126.1923 2020-02-19     1 confirmed
    ## 1661  37.26920  106.1655 2020-02-19     1 confirmed
    ## 1662  35.19170  108.8701 2020-02-19     2 confirmed
    ## 1663  36.34270  118.1498 2020-02-19     1 confirmed
    ## 1664  30.61710  102.7103 2020-02-19     6 confirmed
    ## 1665  23.70000  121.0000 2020-02-19     1 confirmed
    ## 1666  39.30540  117.3230 2020-02-19     2 confirmed
    ## 1667  29.18320  120.0934 2020-02-19     2 confirmed
    ## 1668  32.00000   53.0000 2020-02-19     2     death
    ## 1669  23.34170  113.4244 2020-02-19     1     death
    ## 1670  47.86200  127.7615 2020-02-19     1     death
    ## 1671  22.30000  114.2000 2020-02-19     1     death
    ## 1672  30.97560  112.2707 2020-02-19   108     death
    ## 1673  31.20200  121.4491 2020-02-19     1     death
    ## 1674  24.97400  101.4870 2020-02-19     1     death
    ## 1675  36.00000  138.0000 2020-02-19     5 recovered
    ## 1676   2.50000  112.5000 2020-02-19     2 recovered
    ## 1677   1.28330  103.8333 2020-02-19     5 recovered
    ## 1678  31.82570  117.2264 2020-02-19    52 recovered
    ## 1679  40.18240  116.4142 2020-02-19    23 recovered
    ## 1680  30.05720  107.8740 2020-02-19    20 recovered
    ## 1681  35.44370  139.6380 2020-02-19     1 recovered
    ## 1682  26.07890  117.9874 2020-02-19    19 recovered
    ## 1683  36.06110  103.8343 2020-02-19     3 recovered
    ## 1684  23.34170  113.4244 2020-02-19    41 recovered
    ## 1685  23.82980  108.7881 2020-02-19    17 recovered
    ## 1686  26.81540  106.8748 2020-02-19     4 recovered
    ## 1687  19.19590  109.7453 2020-02-19     5 recovered
    ## 1688  38.04280  114.5149 2020-02-19    16 recovered
    ## 1689  47.86200  127.7615 2020-02-19     9 recovered
    ## 1690  33.88202  113.6140 2020-02-19    51 recovered
    ## 1691  22.30000  114.2000 2020-02-19     3 recovered
    ## 1692  30.97560  112.2707 2020-02-19  1209 recovered
    ## 1693  27.61040  111.7088 2020-02-19    34 recovered
    ## 1694  44.09350  113.9448 2020-02-19     2 recovered
    ## 1695  32.97110  119.4550 2020-02-19    38 recovered
    ## 1696  27.61400  115.7221 2020-02-19    52 recovered
    ## 1697  43.66610  126.1923 2020-02-19     1 recovered
    ## 1698  41.29560  122.6085 2020-02-19     2 recovered
    ## 1699  35.74520   95.9956 2020-02-19     1 recovered
    ## 1700  35.19170  108.8701 2020-02-19    13 recovered
    ## 1701  36.34270  118.1498 2020-02-19    20 recovered
    ## 1702  31.20200  121.4491 2020-02-19     9 recovered
    ## 1703  37.57770  112.2922 2020-02-19     7 recovered
    ## 1704  30.61710  102.7103 2020-02-19    19 recovered
    ## 1705  39.30540  117.3230 2020-02-19     6 recovered
    ## 1706  41.11290   85.2401 2020-02-19     8 recovered
    ## 1707  24.97400  101.4870 2020-02-19     3 recovered
    ## 1708  29.18320  120.0934 2020-02-19    69 recovered
    ## 1709  32.00000   53.0000 2020-02-20     3 confirmed
    ## 1710  36.00000  138.0000 2020-02-20    10 confirmed
    ## 1711  36.00000  128.0000 2020-02-20    73 confirmed
    ## 1712  31.82570  117.2264 2020-02-20     1 confirmed
    ## 1713  40.18240  116.4142 2020-02-20     2 confirmed
    ## 1714  30.05720  107.8740 2020-02-20     7 confirmed
    ## 1715  35.44370  139.6380 2020-02-20    13 confirmed
    ## 1716  23.34170  113.4244 2020-02-20     1 confirmed
    ## 1717  23.82980  108.7881 2020-02-20     1 confirmed
    ## 1718  38.04280  114.5149 2020-02-20     1 confirmed
    ## 1719  47.86200  127.7615 2020-02-20     6 confirmed
    ## 1720  33.88202  113.6140 2020-02-20     3 confirmed
    ## 1721  22.30000  114.2000 2020-02-20     5 confirmed
    ## 1722  30.97560  112.2707 2020-02-20   411 confirmed
    ## 1723  27.61040  111.7088 2020-02-20     2 confirmed
    ## 1724  43.66610  126.1923 2020-02-20     1 confirmed
    ## 1725  35.19170  108.8701 2020-02-20     3 confirmed
    ## 1726  36.34270  118.1498 2020-02-20     2 confirmed
    ## 1727  31.20200  121.4491 2020-02-20     1 confirmed
    ## 1728  37.57770  112.2922 2020-02-20     1 confirmed
    ## 1729  30.61710  102.7103 2020-02-20     6 confirmed
    ## 1730  23.70000  121.0000 2020-02-20     1 confirmed
    ## 1731  39.30540  117.3230 2020-02-20     1 confirmed
    ## 1732  24.97400  101.4870 2020-02-20     2 confirmed
    ## 1733  29.18320  120.0934 2020-02-20     1 confirmed
    ## 1734  36.00000  128.0000 2020-02-20     1     death
    ## 1735  30.05720  107.8740 2020-02-20     1     death
    ## 1736  35.44370  139.6380 2020-02-20     2     death
    ## 1737  26.07890  117.9874 2020-02-20     1     death
    ## 1738  38.04280  114.5149 2020-02-20     1     death
    ## 1739  30.97560  112.2707 2020-02-20   115     death
    ## 1740  35.19170  108.8701 2020-02-20     1     death
    ## 1741  36.34270  118.1498 2020-02-20     1     death
    ## 1742  24.97400  101.4870 2020-02-20     1     death
    ## 1743  29.18320  120.0934 2020-02-20     1     death
    ## 1744  36.00000  128.0000 2020-02-20     4 recovered
    ## 1745  31.82570  117.2264 2020-02-20    61 recovered
    ## 1746  40.18240  116.4142 2020-02-20     8 recovered
    ## 1747  30.05720  107.8740 2020-02-20    25 recovered
    ## 1748  26.07890  117.9874 2020-02-20    14 recovered
    ## 1749  36.06110  103.8343 2020-02-20     6 recovered
    ## 1750  23.34170  113.4244 2020-02-20    36 recovered
    ## 1751  23.82980  108.7881 2020-02-20     4 recovered
    ## 1752  26.81540  106.8748 2020-02-20     2 recovered
    ## 1753  19.19590  109.7453 2020-02-20     2 recovered
    ## 1754  38.04280  114.5149 2020-02-20    17 recovered
    ## 1755  47.86200  127.7615 2020-02-20    16 recovered
    ## 1756  33.88202  113.6140 2020-02-20    64 recovered
    ## 1757  22.30000  114.2000 2020-02-20     1 recovered
    ## 1758  30.97560  112.2707 2020-02-20  1451 recovered
    ## 1759  27.61040  111.7088 2020-02-20    73 recovered
    ## 1760  44.09350  113.9448 2020-02-20     6 recovered
    ## 1761  32.97110  119.4550 2020-02-20    38 recovered
    ## 1762  27.61400  115.7221 2020-02-20    71 recovered
    ## 1763  43.66610  126.1923 2020-02-20     6 recovered
    ## 1764  41.29560  122.6085 2020-02-20     4 recovered
    ## 1765  22.16670  113.5500 2020-02-20     1 recovered
    ## 1766  37.26920  106.1655 2020-02-20     2 recovered
    ## 1767  35.19170  108.8701 2020-02-20    16 recovered
    ## 1768  36.34270  118.1498 2020-02-20    23 recovered
    ## 1769  31.20200  121.4491 2020-02-20    13 recovered
    ## 1770  37.57770  112.2922 2020-02-20     8 recovered
    ## 1771  30.61710  102.7103 2020-02-20    29 recovered
    ## 1772  39.30540  117.3230 2020-02-20     5 recovered
    ## 1773  41.11290   85.2401 2020-02-20     2 recovered
    ## 1774  24.97400  101.4870 2020-02-20    19 recovered
    ## 1775  29.18320  120.0934 2020-02-20    29 recovered
    ## 1776  32.00000   53.0000 2020-02-21    13 confirmed
    ## 1777  31.00000   35.0000 2020-02-21     1 confirmed
    ## 1778  43.00000   12.0000 2020-02-21    17 confirmed
    ## 1779  36.00000  138.0000 2020-02-21    11 confirmed
    ## 1780  33.85470   35.8623 2020-02-21     1 confirmed
    ## 1781   1.28330  103.8333 2020-02-21     1 confirmed
    ## 1782  36.00000  128.0000 2020-02-21   100 confirmed
    ## 1783  31.82570  117.2264 2020-02-21     1 confirmed
    ## 1784  40.18240  116.4142 2020-02-21     1 confirmed
    ## 1785  49.28270 -123.1207 2020-02-21     1 confirmed
    ## 1786  30.05720  107.8740 2020-02-21     5 confirmed
    ## 1787  35.44370  139.6380 2020-02-21     4 confirmed
    ## 1788  23.34170  113.4244 2020-02-21     1 confirmed
    ## 1789  23.82980  108.7881 2020-02-21     1 confirmed
    ## 1790  38.04280  114.5149 2020-02-21     1 confirmed
    ## 1791  47.86200  127.7615 2020-02-21     3 confirmed
    ## 1792  33.88202  113.6140 2020-02-21     2 confirmed
    ## 1793  30.97560  112.2707 2020-02-21   220 confirmed
    ## 1794  40.74500 -123.8695 2020-02-21     1 confirmed
    ## 1795  27.61040  111.7088 2020-02-21     1 confirmed
    ## 1796  29.38290  -98.6134 2020-02-21     2 confirmed
    ## 1797  41.25450  -95.9758 2020-02-21    11 confirmed
    ## 1798  38.47470 -121.3542 2020-02-21     1 confirmed
    ## 1799  36.34270  118.1498 2020-02-21   203 confirmed
    ## 1800  30.61710  102.7103 2020-02-21     5 confirmed
    ## 1801  23.70000  121.0000 2020-02-21     2 confirmed
    ## 1802  39.30540  117.3230 2020-02-21     1 confirmed
    ## 1803  38.27210 -121.9399 2020-02-21     5 confirmed
    ## 1804  29.18320  120.0934 2020-02-21    28 confirmed
    ## 1805  32.00000   53.0000 2020-02-21     2     death
    ## 1806  43.00000   12.0000 2020-02-21     1     death
    ## 1807  36.00000  128.0000 2020-02-21     1     death
    ## 1808  51.00000    9.0000 2020-02-21     2 recovered
    ## 1809  36.00000  138.0000 2020-02-21     4 recovered
    ## 1810   1.28330  103.8333 2020-02-21     3 recovered
    ## 1811  15.00000  101.0000 2020-02-21     2 recovered
    ## 1812  16.00000  108.0000 2020-02-21     7 recovered
    ## 1813  31.82570  117.2264 2020-02-21    65 recovered
    ## 1814  40.18240  116.4142 2020-02-21    16 recovered
    ## 1815  30.05720  107.8740 2020-02-21    17 recovered
    ## 1816  26.07890  117.9874 2020-02-21    23 recovered
    ## 1817  36.06110  103.8343 2020-02-21     5 recovered
    ## 1818  23.34170  113.4244 2020-02-21    48 recovered
    ## 1819  23.82980  108.7881 2020-02-21     7 recovered
    ## 1820  26.81540  106.8748 2020-02-21     5 recovered
    ## 1821  19.19590  109.7453 2020-02-21     9 recovered
    ## 1822  38.04280  114.5149 2020-02-21    15 recovered
    ## 1823  47.86200  127.7615 2020-02-21    39 recovered
    ## 1824  33.88202  113.6140 2020-02-21    99 recovered
    ## 1825  22.30000  114.2000 2020-02-21    -1 recovered
    ## 1826  30.97560  112.2707 2020-02-21    93 recovered
    ## 1827  27.61040  111.7088 2020-02-21    27 recovered
    ## 1828  44.09350  113.9448 2020-02-21     1 recovered
    ## 1829  32.97110  119.4550 2020-02-21    17 recovered
    ## 1830  27.61400  115.7221 2020-02-21    56 recovered
    ## 1831  43.66610  126.1923 2020-02-21     2 recovered
    ## 1832  41.29560  122.6085 2020-02-21     2 recovered
    ## 1833  37.26920  106.1655 2020-02-21     4 recovered
    ## 1834  35.74520   95.9956 2020-02-21     2 recovered
    ## 1835 -28.01670  153.4000 2020-02-21     1 recovered
    ## 1836  32.71570 -117.1611 2020-02-21     1 recovered
    ## 1837  37.35410 -121.9552 2020-02-21     1 recovered
    ## 1838  35.19170  108.8701 2020-02-21    16 recovered
    ## 1839  36.34270  118.1498 2020-02-21    27 recovered
    ## 1840  31.20200  121.4491 2020-02-21    12 recovered
    ## 1841  37.57770  112.2922 2020-02-21     2 recovered
    ## 1842  30.61710  102.7103 2020-02-21    14 recovered
    ## 1843  39.30540  117.3230 2020-02-21     3 recovered
    ## 1844  43.65320  -79.3832 2020-02-21     2 recovered
    ## 1845  41.11290   85.2401 2020-02-21     2 recovered
    ## 1846  24.97400  101.4870 2020-02-21    17 recovered
    ## 1847  29.18320  120.0934 2020-02-21    46 recovered
    ## 1848  32.00000   53.0000 2020-02-22    10 confirmed
    ## 1849  43.00000   12.0000 2020-02-22    42 confirmed
    ## 1850  36.00000  138.0000 2020-02-22    17 confirmed
    ## 1851  36.00000  128.0000 2020-02-22   229 confirmed
    ## 1852  24.00000   54.0000 2020-02-22     4 confirmed
    ## 1853  31.82570  117.2264 2020-02-22     1 confirmed
    ## 1854  40.18240  116.4142 2020-02-22     3 confirmed
    ## 1855  30.05720  107.8740 2020-02-22     1 confirmed
    ## 1856  35.44370  139.6380 2020-02-22     3 confirmed
    ## 1857  23.34170  113.4244 2020-02-22     6 confirmed
    ## 1858  23.82980  108.7881 2020-02-22     3 confirmed
    ## 1859  38.04280  114.5149 2020-02-22     1 confirmed
    ## 1860  33.88202  113.6140 2020-02-22     3 confirmed
    ## 1861  22.30000  114.2000 2020-02-22     1 confirmed
    ## 1862  30.97560  112.2707 2020-02-22  1422 confirmed
    ## 1863  27.61040  111.7088 2020-02-22     2 confirmed
    ## 1864  36.34270  118.1498 2020-02-22     1 confirmed
    ## 1865  31.20200  121.4491 2020-02-22     1 confirmed
    ## 1866  30.61710  102.7103 2020-02-22     1 confirmed
    ## 1867  39.30540  117.3230 2020-02-22     3 confirmed
    ## 1868  29.18320  120.0934 2020-02-22     2 confirmed
    ## 1869  32.00000   53.0000 2020-02-22     1     death
    ## 1870  43.00000   12.0000 2020-02-22     1     death
    ## 1871  38.04280  114.5149 2020-02-22     1     death
    ## 1872  30.97560  112.2707 2020-02-22   202     death
    ## 1873  31.20200  121.4491 2020-02-22     1     death
    ## 1874  41.11290   85.2401 2020-02-22     1     death
    ## 1875  43.00000   12.0000 2020-02-22     1 recovered
    ## 1876  31.82570  117.2264 2020-02-22    58 recovered
    ## 1877  40.18240  116.4142 2020-02-22     9 recovered
    ## 1878  30.05720  107.8740 2020-02-22    12 recovered
    ## 1879  26.07890  117.9874 2020-02-22    13 recovered
    ## 1880  23.34170  113.4244 2020-02-22    38 recovered
    ## 1881  23.82980  108.7881 2020-02-22     7 recovered
    ## 1882  26.81540  106.8748 2020-02-22    13 recovered
    ## 1883  19.19590  109.7453 2020-02-22     9 recovered
    ## 1884  38.04280  114.5149 2020-02-22    19 recovered
    ## 1885  47.86200  127.7615 2020-02-22    29 recovered
    ## 1886  33.88202  113.6140 2020-02-22    94 recovered
    ## 1887  22.30000  114.2000 2020-02-22     1 recovered
    ## 1888  30.97560  112.2707 2020-02-22  3418 recovered
    ## 1889  27.61040  111.7088 2020-02-22    31 recovered
    ## 1890  44.09350  113.9448 2020-02-22     9 recovered
    ## 1891  32.97110  119.4550 2020-02-22    28 recovered
    ## 1892  27.61400  115.7221 2020-02-22    66 recovered
    ## 1893  43.66610  126.1923 2020-02-22     7 recovered
    ## 1894  41.29560  122.6085 2020-02-22     5 recovered
    ## 1895  35.19170  108.8701 2020-02-22    15 recovered
    ## 1896  36.34270  118.1498 2020-02-22    21 recovered
    ## 1897  31.20200  121.4491 2020-02-22    16 recovered
    ## 1898  37.57770  112.2922 2020-02-22     3 recovered
    ## 1899  30.61710  102.7103 2020-02-22    19 recovered
    ## 1900  39.30540  117.3230 2020-02-22     3 recovered
    ## 1901  41.11290   85.2401 2020-02-22     1 recovered
    ## 1902  24.97400  101.4870 2020-02-22    11 recovered
    ## 1903  29.18320  120.0934 2020-02-22    40 recovered
    ## 1904  32.00000   53.0000 2020-02-23    15 confirmed
    ## 1905  43.00000   12.0000 2020-02-23    93 confirmed
    ## 1906  36.00000  138.0000 2020-02-23    25 confirmed
    ## 1907   1.28330  103.8333 2020-02-23     4 confirmed
    ## 1908  36.00000  128.0000 2020-02-23   169 confirmed
    ## 1909  30.05720  107.8740 2020-02-23     2 confirmed
    ## 1910  35.44370  139.6380 2020-02-23    57 confirmed
    ## 1911  23.34170  113.4244 2020-02-23     3 confirmed
    ## 1912  38.04280  114.5149 2020-02-23     2 confirmed
    ## 1913  47.86200  127.7615 2020-02-23     1 confirmed
    ## 1914  33.88202  113.6140 2020-02-23     1 confirmed
    ## 1915  22.30000  114.2000 2020-02-23     5 confirmed
    ## 1916  27.61040  111.7088 2020-02-23     3 confirmed
    ## 1917  36.34270  118.1498 2020-02-23     4 confirmed
    ## 1918  23.70000  121.0000 2020-02-23     2 confirmed
    ## 1919  32.00000   53.0000 2020-02-23     3     death
    ## 1920  43.00000   12.0000 2020-02-23     1     death
    ## 1921  36.00000  128.0000 2020-02-23     4     death
    ## 1922  35.44370  139.6380 2020-02-23     1     death
    ## 1923  23.34170  113.4244 2020-02-23     1     death
    ## 1924  19.19590  109.7453 2020-02-23     1     death
    ## 1925  43.00000   12.0000 2020-02-23     1 recovered
    ## 1926   1.28330  103.8333 2020-02-23    14 recovered
    ## 1927  36.00000  128.0000 2020-02-23     2 recovered
    ## 1928  15.00000  101.0000 2020-02-23     4 recovered
    ## 1929  31.82570  117.2264 2020-02-23    40 recovered
    ## 1930  40.18240  116.4142 2020-02-23    11 recovered
    ## 1931  30.05720  107.8740 2020-02-23     7 recovered
    ## 1932  35.44370  139.6380 2020-02-23    -1 recovered
    ## 1933  26.07890  117.9874 2020-02-23     8 recovered
    ## 1934  36.06110  103.8343 2020-02-23     2 recovered
    ## 1935  23.34170  113.4244 2020-02-23    27 recovered
    ## 1936  23.82980  108.7881 2020-02-23     2 recovered
    ## 1937  26.81540  106.8748 2020-02-23    12 recovered
    ## 1938  19.19590  109.7453 2020-02-23     2 recovered
    ## 1939  38.04280  114.5149 2020-02-23    16 recovered
    ## 1940  47.86200  127.7615 2020-02-23    18 recovered
    ## 1941  33.88202  113.6140 2020-02-23    38 recovered
    ## 1942  22.30000  114.2000 2020-02-23     5 recovered
    ## 1943  30.97560  112.2707 2020-02-23    44 recovered
    ## 1944  27.61040  111.7088 2020-02-23    22 recovered
    ## 1945  44.09350  113.9448 2020-02-23     1 recovered
    ## 1946  32.97110  119.4550 2020-02-23    17 recovered
    ## 1947  27.61400  115.7221 2020-02-23    58 recovered
    ## 1948  43.66610  126.1923 2020-02-23     2 recovered
    ## 1949  41.29560  122.6085 2020-02-23     7 recovered
    ## 1950  37.26920  106.1655 2020-02-23     8 recovered
    ## 1951  35.19170  108.8701 2020-02-23    14 recovered
    ## 1952  36.34270  118.1498 2020-02-23    19 recovered
    ## 1953  31.20200  121.4491 2020-02-23    22 recovered
    ## 1954  37.57770  112.2922 2020-02-23     7 recovered
    ## 1955  30.61710  102.7103 2020-02-23    11 recovered
    ## 1956  39.30540  117.3230 2020-02-23    16 recovered
    ## 1957  41.11290   85.2401 2020-02-23     3 recovered
    ## 1958  24.97400  101.4870 2020-02-23     8 recovered
    ## 1959  29.18320  120.0934 2020-02-23    41 recovered
    ## 1960  33.00000   65.0000 2020-02-24     1 confirmed
    ## 1961  26.02750   50.5500 2020-02-24     1 confirmed
    ## 1962  32.00000   53.0000 2020-02-24    18 confirmed
    ## 1963  33.00000   44.0000 2020-02-24     1 confirmed
    ## 1964  43.00000   12.0000 2020-02-24    74 confirmed
    ## 1965  36.00000  138.0000 2020-02-24    12 confirmed
    ## 1966  29.50000   47.7500 2020-02-24     1 confirmed
    ## 1967  21.00000   57.0000 2020-02-24     2 confirmed
    ## 1968  36.00000  128.0000 2020-02-24   231 confirmed
    ## 1969  55.00000   -3.0000 2020-02-24     4 confirmed
    ## 1970  30.05720  107.8740 2020-02-24     1 confirmed
    ## 1971  23.34170  113.4244 2020-02-24     3 confirmed
    ## 1972  23.82980  108.7881 2020-02-24     2 confirmed
    ## 1973  22.30000  114.2000 2020-02-24     5 confirmed
    ## 1974  30.97560  112.2707 2020-02-24   203 confirmed
    ## 1975  43.66610  126.1923 2020-02-24     2 confirmed
    ## 1976  29.38290  -98.6134 2020-02-24    -2 confirmed
    ## 1977  41.25450  -95.9758 2020-02-24   -11 confirmed
    ## 1978  36.34270  118.1498 2020-02-24     1 confirmed
    ## 1979  37.57770  112.2922 2020-02-24     1 confirmed
    ## 1980  30.61710  102.7103 2020-02-24     1 confirmed
    ## 1981  23.70000  121.0000 2020-02-24     2 confirmed
    ## 1982  43.65320  -79.3832 2020-02-24     1 confirmed
    ## 1983  38.27210 -121.9399 2020-02-24    -5 confirmed
    ## 1984  35.44370  139.6380 2020-02-24    36 confirmed
    ## 1985  32.00000   53.0000 2020-02-24     4     death
    ## 1986  43.00000   12.0000 2020-02-24     4     death
    ## 1987  36.00000  128.0000 2020-02-24     2     death
    ## 1988  30.97560  112.2707 2020-02-24   149     death
    ## 1989  36.34270  118.1498 2020-02-24     1     death
    ## 1990  43.00000   12.0000 2020-02-24    -1 recovered
    ## 1991   2.50000  112.5000 2020-02-24     3 recovered
    ## 1992  31.82570  117.2264 2020-02-24    26 recovered
    ## 1993  40.18240  116.4142 2020-02-24     9 recovered
    ## 1994  30.05720  107.8740 2020-02-24    14 recovered
    ## 1995  26.07890  117.9874 2020-02-24    13 recovered
    ## 1996  36.06110  103.8343 2020-02-24     2 recovered
    ## 1997  23.34170  113.4244 2020-02-24    31 recovered
    ## 1998  23.82980  108.7881 2020-02-24     6 recovered
    ## 1999  19.19590  109.7453 2020-02-24    10 recovered
    ## 2000  38.04280  114.5149 2020-02-24    15 recovered
    ## 2001  47.86200  127.7615 2020-02-24     5 recovered
    ## 2002  33.88202  113.6140 2020-02-24    75 recovered
    ## 2003  22.30000  114.2000 2020-02-24     8 recovered
    ## 2004  30.97560  112.2707 2020-02-24  1405 recovered
    ## 2005  27.61040  111.7088 2020-02-24    17 recovered
    ## 2006  44.09350  113.9448 2020-02-24     7 recovered
    ## 2007  32.97110  119.4550 2020-02-24    34 recovered
    ## 2008  27.61400  115.7221 2020-02-24    32 recovered
    ## 2009  43.66610  126.1923 2020-02-24     6 recovered
    ## 2010  41.29560  122.6085 2020-02-24     7 recovered
    ## 2011  37.26920  106.1655 2020-02-24     2 recovered
    ## 2012  35.19170  108.8701 2020-02-24    10 recovered
    ## 2013  36.34270  118.1498 2020-02-24    22 recovered
    ## 2014  31.20200  121.4491 2020-02-24    12 recovered
    ## 2015  37.57770  112.2922 2020-02-24     6 recovered
    ## 2016  30.61710  102.7103 2020-02-24    15 recovered
    ## 2017  23.70000  121.0000 2020-02-24     3 recovered
    ## 2018  39.30540  117.3230 2020-02-24     6 recovered
    ## 2019  41.11290   85.2401 2020-02-24     2 recovered
    ## 2020  24.97400  101.4870 2020-02-24     9 recovered
    ## 2021  29.18320  120.0934 2020-02-24    22 recovered
    ## 2022  28.03390    1.6596 2020-02-25     1 confirmed
    ## 2023  47.51620   14.5501 2020-02-25     2 confirmed
    ## 2024  26.02750   50.5500 2020-02-25    22 confirmed
    ## 2025  45.10000   15.2000 2020-02-25     1 confirmed
    ## 2026  47.00000    2.0000 2020-02-25     2 confirmed
    ## 2027  51.00000    9.0000 2020-02-25     1 confirmed
    ## 2028  32.00000   53.0000 2020-02-25    34 confirmed
    ## 2029  43.00000   12.0000 2020-02-25    93 confirmed
    ## 2030  36.00000  138.0000 2020-02-25    11 confirmed
    ## 2031  29.50000   47.7500 2020-02-25    10 confirmed
    ## 2032   1.28330  103.8333 2020-02-25     2 confirmed
    ## 2033  36.00000  128.0000 2020-02-25   144 confirmed
    ## 2034  40.00000   -4.0000 2020-02-25     4 confirmed
    ## 2035  46.81820    8.2275 2020-02-25     1 confirmed
    ## 2036  15.00000  101.0000 2020-02-25     2 confirmed
    ## 2037  40.18240  116.4142 2020-02-25     1 confirmed
    ## 2038  49.28270 -123.1207 2020-02-25     1 confirmed
    ## 2039  26.07890  117.9874 2020-02-25     1 confirmed
    ## 2040  23.34170  113.4244 2020-02-25     2 confirmed
    ## 2041  23.82980  108.7881 2020-02-25     1 confirmed
    ## 2042  22.30000  114.2000 2020-02-25     5 confirmed
    ## 2043  30.97560  112.2707 2020-02-25   499 confirmed
    ## 2044  36.34270  118.1498 2020-02-25     1 confirmed
    ## 2045  31.20200  121.4491 2020-02-25     1 confirmed
    ## 2046  30.61710  102.7103 2020-02-25     2 confirmed
    ## 2047  23.70000  121.0000 2020-02-25     1 confirmed
    ## 2048  32.00000   53.0000 2020-02-25     4     death
    ## 2049  43.00000   12.0000 2020-02-25     3     death
    ## 2050  36.00000  128.0000 2020-02-25     2     death
    ## 2051  23.34170  113.4244 2020-02-25     1     death
    ## 2052  30.97560  112.2707 2020-02-25    68     death
    ## 2053  36.34270  118.1498 2020-02-25     1     death
    ## 2054  47.00000    2.0000 2020-02-25     7 recovered
    ## 2055   1.28330  103.8333 2020-02-25     2 recovered
    ## 2056  36.00000  128.0000 2020-02-25     4 recovered
    ## 2057  15.00000  101.0000 2020-02-25     1 recovered
    ## 2058  16.00000  108.0000 2020-02-25     2 recovered
    ## 2059  31.82570  117.2264 2020-02-25    49 recovered
    ## 2060  40.18240  116.4142 2020-02-25    17 recovered
    ## 2061  30.05720  107.8740 2020-02-25    23 recovered
    ## 2062  26.07890  117.9874 2020-02-25    16 recovered
    ## 2063  23.34170  113.4244 2020-02-25    36 recovered
    ## 2064  23.82980  108.7881 2020-02-25    22 recovered
    ## 2065  26.81540  106.8748 2020-02-25     2 recovered
    ## 2066  19.19590  109.7453 2020-02-25     8 recovered
    ## 2067  38.04280  114.5149 2020-02-25    14 recovered
    ## 2068  47.86200  127.7615 2020-02-25    16 recovered
    ## 2069  33.88202  113.6140 2020-02-25    59 recovered
    ## 2070  30.97560  112.2707 2020-02-25  2223 recovered
    ## 2071  27.61040  111.7088 2020-02-25    37 recovered
    ## 2072  44.09350  113.9448 2020-02-25     1 recovered
    ## 2073  32.97110  119.4550 2020-02-25     6 recovered
    ## 2074  27.61400  115.7221 2020-02-25    38 recovered
    ## 2075  43.66610  126.1923 2020-02-25     3 recovered
    ## 2076  41.29560  122.6085 2020-02-25     3 recovered
    ## 2077  22.16670  113.5500 2020-02-25     1 recovered
    ## 2078  37.26920  106.1655 2020-02-25     3 recovered
    ## 2079  35.19170  108.8701 2020-02-25    13 recovered
    ## 2080  36.34270  118.1498 2020-02-25    12 recovered
    ## 2081  31.20200  121.4491 2020-02-25     7 recovered
    ## 2082  37.57770  112.2922 2020-02-25     4 recovered
    ## 2083  30.61710  102.7103 2020-02-25    13 recovered
    ## 2084  33.42550 -111.9400 2020-02-25     1 recovered
    ## 2085  39.30540  117.3230 2020-02-25     4 recovered
    ## 2086  24.97400  101.4870 2020-02-25     5 recovered
    ## 2087  29.18320  120.0934 2020-02-25    26 recovered
    ## 2088  26.02750   50.5500 2020-02-26    10 confirmed
    ## 2089 -14.23500  -51.9253 2020-02-26     1 confirmed
    ## 2090  45.10000   15.2000 2020-02-26     2 confirmed
    ## 2091  64.00000   26.0000 2020-02-26     1 confirmed
    ## 2092  47.00000    2.0000 2020-02-26     4 confirmed
    ## 2093  42.31540   43.3569 2020-02-26     1 confirmed
    ## 2094  51.00000    9.0000 2020-02-26    10 confirmed
    ## 2095  39.07420   21.8243 2020-02-26     1 confirmed
    ## 2096  32.00000   53.0000 2020-02-26    44 confirmed
    ## 2097  33.00000   44.0000 2020-02-26     4 confirmed
    ## 2098  31.00000   35.0000 2020-02-26     1 confirmed
    ## 2099  43.00000   12.0000 2020-02-26   131 confirmed
    ## 2100  36.00000  138.0000 2020-02-26    19 confirmed
    ## 2101  29.50000   47.7500 2020-02-26    15 confirmed
    ## 2102  33.85470   35.8623 2020-02-26     1 confirmed
    ## 2103  41.60860   21.7453 2020-02-26     1 confirmed
    ## 2104  60.47200    8.4689 2020-02-26     1 confirmed
    ## 2105  21.00000   57.0000 2020-02-26     2 confirmed
    ## 2106  30.37530   69.3451 2020-02-26     2 confirmed
    ## 2107  45.94320   24.9668 2020-02-26     1 confirmed
    ## 2108   1.28330  103.8333 2020-02-26     2 confirmed
    ## 2109  36.00000  128.0000 2020-02-26   284 confirmed
    ## 2110  40.00000   -4.0000 2020-02-26     7 confirmed
    ## 2111  63.00000   16.0000 2020-02-26     1 confirmed
    ## 2112  15.00000  101.0000 2020-02-26     3 confirmed
    ## 2113  35.44370  139.6380 2020-02-26    14 confirmed
    ## 2114  38.04280  114.5149 2020-02-26     1 confirmed
    ## 2115  22.30000  114.2000 2020-02-26     7 confirmed
    ## 2116  30.97560  112.2707 2020-02-26   401 confirmed
    ## 2117  31.20200  121.4491 2020-02-26     1 confirmed
    ## 2118  30.61710  102.7103 2020-02-26     2 confirmed
    ## 2119  23.70000  121.0000 2020-02-26     1 confirmed
    ## 2120  35.44370  139.6380 2020-02-26     6 confirmed
    ## 2121  47.00000    2.0000 2020-02-26     1     death
    ## 2122  32.00000   53.0000 2020-02-26     3     death
    ## 2123  43.00000   12.0000 2020-02-26     2     death
    ## 2124  36.00000  138.0000 2020-02-26     1     death
    ## 2125  36.00000  128.0000 2020-02-26     2     death
    ## 2126  35.44370  139.6380 2020-02-26     1     death
    ## 2127  30.97560  112.2707 2020-02-26    52     death
    ## 2128  51.00000    9.0000 2020-02-26     1 recovered
    ## 2129  32.00000   53.0000 2020-02-26    49 recovered
    ## 2130  43.00000   12.0000 2020-02-26     2 recovered
    ## 2131   1.28330  103.8333 2020-02-26     9 recovered
    ## 2132  31.82570  117.2264 2020-02-26    32 recovered
    ## 2133  40.18240  116.4142 2020-02-26    20 recovered
    ## 2134  30.05720  107.8740 2020-02-26    12 recovered
    ## 2135  35.44370  139.6380 2020-02-26    10 recovered
    ## 2136  26.07890  117.9874 2020-02-26    19 recovered
    ## 2137  36.06110  103.8343 2020-02-26     1 recovered
    ## 2138  23.34170  113.4244 2020-02-26    29 recovered
    ## 2139  23.82980  108.7881 2020-02-26    13 recovered
    ## 2140  19.19590  109.7453 2020-02-26     5 recovered
    ## 2141  38.04280  114.5149 2020-02-26    13 recovered
    ## 2142  47.86200  127.7615 2020-02-26     6 recovered
    ## 2143  33.88202  113.6140 2020-02-26    31 recovered
    ## 2144  22.30000  114.2000 2020-02-26     5 recovered
    ## 2145  30.97560  112.2707 2020-02-26  1998 recovered
    ## 2146  27.61040  111.7088 2020-02-26    15 recovered
    ## 2147  44.09350  113.9448 2020-02-26     3 recovered
    ## 2148  32.97110  119.4550 2020-02-26    20 recovered
    ## 2149  27.61400  115.7221 2020-02-26    36 recovered
    ## 2150  43.66610  126.1923 2020-02-26     2 recovered
    ## 2151  41.29560  122.6085 2020-02-26     5 recovered
    ## 2152  37.26920  106.1655 2020-02-26     4 recovered
    ## 2153  35.19170  108.8701 2020-02-26     6 recovered
    ## 2154  36.34270  118.1498 2020-02-26    22 recovered
    ## 2155  31.20200  121.4491 2020-02-26     4 recovered
    ## 2156  37.57770  112.2922 2020-02-26     6 recovered
    ## 2157  30.61710  102.7103 2020-02-26    18 recovered
    ## 2158  39.30540  117.3230 2020-02-26     5 recovered
    ## 2159  41.11290   85.2401 2020-02-26     4 recovered
    ## 2160  24.97400  101.4870 2020-02-26    15 recovered
    ## 2161  29.18320  120.0934 2020-02-26    59 recovered
    ## 2162  47.51620   14.5501 2020-02-27     1 confirmed
    ## 2163  56.26390    9.5018 2020-02-27     1 confirmed
    ## 2164  58.59530   25.0136 2020-02-27     1 confirmed
    ## 2165  47.00000    2.0000 2020-02-27    20 confirmed
    ## 2166  51.00000    9.0000 2020-02-27    19 confirmed
    ## 2167  39.07420   21.8243 2020-02-27     2 confirmed
    ## 2168  32.00000   53.0000 2020-02-27   106 confirmed
    ## 2169  33.00000   44.0000 2020-02-27     2 confirmed
    ## 2170  31.00000   35.0000 2020-02-27     1 confirmed
    ## 2171  43.00000   12.0000 2020-02-27   202 confirmed
    ## 2172  36.00000  138.0000 2020-02-27    25 confirmed
    ## 2173  29.50000   47.7500 2020-02-27    17 confirmed
    ## 2174   2.50000  112.5000 2020-02-27     1 confirmed
    ## 2175  52.13260    5.2913 2020-02-27     1 confirmed
    ## 2176  43.94240   12.4578 2020-02-27     1 confirmed
    ## 2177  36.00000  128.0000 2020-02-27   505 confirmed
    ## 2178  40.00000   -4.0000 2020-02-27     2 confirmed
    ## 2179  63.00000   16.0000 2020-02-27     5 confirmed
    ## 2180  46.81820    8.2275 2020-02-27     7 confirmed
    ## 2181  55.00000   -3.0000 2020-02-27     2 confirmed
    ## 2182  40.18240  116.4142 2020-02-27    10 confirmed
    ## 2183  35.44370  139.6380 2020-02-27     1 confirmed
    ## 2184  26.07890  117.9874 2020-02-27     2 confirmed
    ## 2185  38.04280  114.5149 2020-02-27     5 confirmed
    ## 2186  33.88202  113.6140 2020-02-27     1 confirmed
    ## 2187  22.30000  114.2000 2020-02-27     1 confirmed
    ## 2188  30.97560  112.2707 2020-02-27   409 confirmed
    ## 2189  27.61040  111.7088 2020-02-27     1 confirmed
    ## 2190  37.26920  106.1655 2020-02-27     1 confirmed
    ## 2191  38.47470 -121.3542 2020-02-27     1 confirmed
    ## 2192  30.61710  102.7103 2020-02-27     3 confirmed
    ## 2193  39.30540  117.3230 2020-02-27     1 confirmed
    ## 2194  43.65320  -79.3832 2020-02-27     2 confirmed
    ## 2195  32.00000   53.0000 2020-02-27     7     death
    ## 2196  43.00000   12.0000 2020-02-27     5     death
    ## 2197  36.00000  138.0000 2020-02-27     2     death
    ## 2198  36.00000  128.0000 2020-02-27     1     death
    ## 2199  40.18240  116.4142 2020-02-27     1     death
    ## 2200  47.86200  127.7615 2020-02-27     1     death
    ## 2201  33.88202  113.6140 2020-02-27     1     death
    ## 2202  30.97560  112.2707 2020-02-27    26     death
    ## 2203  51.00000    9.0000 2020-02-27     1 recovered
    ## 2204  31.00000   35.0000 2020-02-27     1 recovered
    ## 2205  43.00000   12.0000 2020-02-27    42 recovered
    ## 2206  31.82570  117.2264 2020-02-27    48 recovered
    ## 2207  40.18240  116.4142 2020-02-27    13 recovered
    ## 2208  49.28270 -123.1207 2020-02-27     3 recovered
    ## 2209  30.05720  107.8740 2020-02-27    17 recovered
    ## 2210  26.07890  117.9874 2020-02-27    10 recovered
    ## 2211  23.34170  113.4244 2020-02-27    39 recovered
    ## 2212  23.82980  108.7881 2020-02-27    14 recovered
    ## 2213  26.81540  106.8748 2020-02-27     8 recovered
    ## 2214  19.19590  109.7453 2020-02-27     2 recovered
    ## 2215  38.04280  114.5149 2020-02-27    13 recovered
    ## 2216  47.86200  127.7615 2020-02-27    21 recovered
    ## 2217  33.88202  113.6140 2020-02-27    35 recovered
    ## 2218  30.97560  112.2707 2020-02-27  2414 recovered
    ## 2219  27.61040  111.7088 2020-02-27    21 recovered
    ## 2220  44.09350  113.9448 2020-02-27     5 recovered
    ## 2221  32.97110  119.4550 2020-02-27    20 recovered
    ## 2222  27.61400  115.7221 2020-02-27    35 recovered
    ## 2223  43.66610  126.1923 2020-02-27     2 recovered
    ## 2224  41.29560  122.6085 2020-02-27     5 recovered
    ## 2225  22.16670  113.5500 2020-02-27     1 recovered
    ## 2226  37.26920  106.1655 2020-02-27     3 recovered
    ## 2227  35.19170  108.8701 2020-02-27     3 recovered
    ## 2228  36.34270  118.1498 2020-02-27    10 recovered
    ## 2229  31.20200  121.4491 2020-02-27     4 recovered
    ## 2230  37.57770  112.2922 2020-02-27     3 recovered
    ## 2231  30.61710  102.7103 2020-02-27    14 recovered
    ## 2232  39.30540  117.3230 2020-02-27     6 recovered
    ## 2233  41.11290   85.2401 2020-02-27     9 recovered
    ## 2234  24.97400  101.4870 2020-02-27     6 recovered
    ## 2235  29.18320  120.0934 2020-02-27    65 recovered
    ## 2236  26.02750   50.5500 2020-02-28     3 confirmed
    ## 2237  53.70980   27.9534 2020-02-28     1 confirmed
    ## 2238  45.10000   15.2000 2020-02-28     2 confirmed
    ## 2239  47.00000    2.0000 2020-02-28    19 confirmed
    ## 2240  51.00000    9.0000 2020-02-28     2 confirmed
    ## 2241  39.07420   21.8243 2020-02-28     1 confirmed
    ## 2242  64.96310  -19.0208 2020-02-28     1 confirmed
    ## 2243  32.00000   53.0000 2020-02-28   143 confirmed
    ## 2244  31.00000   35.0000 2020-02-28     1 confirmed
    ## 2245  43.00000   12.0000 2020-02-28   233 confirmed
    ## 2246  36.00000  138.0000 2020-02-28    14 confirmed
    ## 2247  29.50000   47.7500 2020-02-28     2 confirmed
    ## 2248  55.16940   23.8813 2020-02-28     1 confirmed
    ## 2249  23.63450 -102.5528 2020-02-28     1 confirmed
    ## 2250 -40.90060  174.8860 2020-02-28     1 confirmed
    ## 2251   9.08200    8.6753 2020-02-28     1 confirmed
    ## 2252  60.47200    8.4689 2020-02-28     5 confirmed
    ## 2253  45.94320   24.9668 2020-02-28     2 confirmed
    ## 2254  36.00000  128.0000 2020-02-28   571 confirmed
    ## 2255  40.00000   -4.0000 2020-02-28    17 confirmed
    ## 2256  15.00000  101.0000 2020-02-28     1 confirmed
    ## 2257  55.00000   -3.0000 2020-02-28     5 confirmed
    ## 2258  24.00000   54.0000 2020-02-28     6 confirmed
    ## 2259  45.50170  -73.5673 2020-02-28     1 confirmed
    ## 2260  31.82570  117.2264 2020-02-28     1 confirmed
    ## 2261  23.34170  113.4244 2020-02-28     1 confirmed
    ## 2262  38.04280  114.5149 2020-02-28     1 confirmed
    ## 2263  22.30000  114.2000 2020-02-28     2 confirmed
    ## 2264  30.97560  112.2707 2020-02-28   318 confirmed
    ## 2265  27.61400  115.7221 2020-02-28     1 confirmed
    ## 2266  30.61710  102.7103 2020-02-28     4 confirmed
    ## 2267  23.70000  121.0000 2020-02-28     2 confirmed
    ## 2268  35.44370  139.6380 2020-02-28     2 confirmed
    ## 2269  32.00000   53.0000 2020-02-28     8     death
    ## 2270  43.00000   12.0000 2020-02-28     4     death
    ## 2271  40.18240  116.4142 2020-02-28     2     death
    ## 2272  35.44370  139.6380 2020-02-28     2     death
    ## 2273  30.97560  112.2707 2020-02-28    41     death
    ## 2274  41.11290   85.2401 2020-02-28     1     death
    ## 2275  26.00000   30.0000 2020-02-28     1 recovered
    ## 2276  32.00000   53.0000 2020-02-28    24 recovered
    ## 2277  43.00000   12.0000 2020-02-28     1 recovered
    ## 2278  15.00000  101.0000 2020-02-28     6 recovered
    ## 2279  24.00000   54.0000 2020-02-28     1 recovered
    ## 2280  31.82570  117.2264 2020-02-28    29 recovered
    ## 2281  40.18240  116.4142 2020-02-28     9 recovered
    ## 2282  42.36010  -71.0589 2020-02-28     1 recovered
    ## 2283  30.05720  107.8740 2020-02-28    21 recovered
    ## 2284  26.07890  117.9874 2020-02-28     7 recovered
    ## 2285  36.06110  103.8343 2020-02-28     1 recovered
    ## 2286  23.34170  113.4244 2020-02-28    45 recovered
    ## 2287  23.82980  108.7881 2020-02-28     7 recovered
    ## 2288  19.19590  109.7453 2020-02-28     2 recovered
    ## 2289  38.04280  114.5149 2020-02-28     3 recovered
    ## 2290  47.86200  127.7615 2020-02-28    13 recovered
    ## 2291  33.88202  113.6140 2020-02-28    44 recovered
    ## 2292  22.30000  114.2000 2020-02-28     6 recovered
    ## 2293  30.97560  112.2707 2020-02-28  3020 recovered
    ## 2294  27.61040  111.7088 2020-02-28    26 recovered
    ## 2295  44.09350  113.9448 2020-02-28     2 recovered
    ## 2296  32.97110  119.4550 2020-02-28    17 recovered
    ## 2297  27.61400  115.7221 2020-02-28    36 recovered
    ## 2298  43.66610  126.1923 2020-02-28     6 recovered
    ## 2299  35.19170  108.8701 2020-02-28     4 recovered
    ## 2300  36.34270  118.1498 2020-02-28    18 recovered
    ## 2301  31.20200  121.4491 2020-02-28     3 recovered
    ## 2302  37.57770  112.2922 2020-02-28     5 recovered
    ## 2303  30.61710  102.7103 2020-02-28    17 recovered
    ## 2304  23.70000  121.0000 2020-02-28     1 recovered
    ## 2305  41.11290   85.2401 2020-02-28     9 recovered
    ## 2306  24.97400  101.4870 2020-02-28     6 recovered
    ## 2307  29.18320  120.0934 2020-02-28    43 recovered
    ## 2308  47.51620   14.5501 2020-02-29     6 confirmed
    ## 2309  26.02750   50.5500 2020-02-29     5 confirmed
    ## 2310 -14.23500  -51.9253 2020-02-29     1 confirmed
    ## 2311  45.10000   15.2000 2020-02-29     1 confirmed
    ## 2312  56.26390    9.5018 2020-02-29     2 confirmed
    ## 2313  64.00000   26.0000 2020-02-29     1 confirmed
    ## 2314  47.00000    2.0000 2020-02-29    43 confirmed
    ## 2315  51.00000    9.0000 2020-02-29    31 confirmed
    ## 2316  32.00000   53.0000 2020-02-29   205 confirmed
    ## 2317  33.00000   44.0000 2020-02-29     6 confirmed
    ## 2318  53.14240   -7.6921 2020-02-29     1 confirmed
    ## 2319  31.00000   35.0000 2020-02-29     3 confirmed
    ## 2320  43.00000   12.0000 2020-02-29   240 confirmed
    ## 2321  36.00000  138.0000 2020-02-29    13 confirmed
    ## 2322  33.85470   35.8623 2020-02-29     2 confirmed
    ## 2323  49.81530    6.1296 2020-02-29     1 confirmed
    ## 2324   2.50000  112.5000 2020-02-29     2 confirmed
    ## 2325  23.63450 -102.5528 2020-02-29     3 confirmed
    ## 2326  43.73330    7.4167 2020-02-29     1 confirmed
    ## 2327  52.13260    5.2913 2020-02-29     5 confirmed
    ## 2328  60.47200    8.4689 2020-02-29     9 confirmed
    ## 2329  21.00000   57.0000 2020-02-29     2 confirmed
    ## 2330  30.37530   69.3451 2020-02-29     2 confirmed
    ## 2331  25.35480   51.1839 2020-02-29     1 confirmed
    ## 2332   1.28330  103.8333 2020-02-29     9 confirmed
    ## 2333  36.00000  128.0000 2020-02-29   813 confirmed
    ## 2334  40.00000   -4.0000 2020-02-29    13 confirmed
    ## 2335  63.00000   16.0000 2020-02-29     5 confirmed
    ## 2336  46.81820    8.2275 2020-02-29    10 confirmed
    ## 2337  15.00000  101.0000 2020-02-29     1 confirmed
    ## 2338  55.00000   -3.0000 2020-02-29     3 confirmed
    ## 2339  24.00000   54.0000 2020-02-29     2 confirmed
    ## 2340  40.18240  116.4142 2020-02-29     1 confirmed
    ## 2341  49.28270 -123.1207 2020-02-29     1 confirmed
    ## 2342  35.44370  139.6380 2020-02-29    -8 confirmed
    ## 2343  23.34170  113.4244 2020-02-29     1 confirmed
    ## 2344  22.30000  114.2000 2020-02-29     1 confirmed
    ## 2345  30.97560  112.2707 2020-02-29   423 confirmed
    ## 2346  27.61040  111.7088 2020-02-29     1 confirmed
    ## 2347  47.60620 -122.3321 2020-02-29     5 confirmed
    ## 2348  37.26920  106.1655 2020-02-29     1 confirmed
    ## 2349 -28.01670  153.4000 2020-02-29     4 confirmed
    ## 2350  37.35410 -121.9552 2020-02-29     1 confirmed
    ## 2351  48.03300 -121.8339 2020-02-29     1 confirmed
    ## 2352 -34.92850  138.6007 2020-02-29     1 confirmed
    ## 2353  23.70000  121.0000 2020-02-29     5 confirmed
    ## 2354  43.65320  -79.3832 2020-02-29     5 confirmed
    ## 2355 -37.81360  144.9631 2020-02-29     3 confirmed
    ## 2356  45.54700 -123.1386 2020-02-29     1 confirmed
    ## 2357 -31.95050  115.8605 2020-02-29     2 confirmed
    ## 2358  32.00000   53.0000 2020-02-29     9     death
    ## 2359  43.00000   12.0000 2020-02-29     8     death
    ## 2360  36.00000  138.0000 2020-02-29     1     death
    ## 2361  36.00000  128.0000 2020-02-29     3     death
    ## 2362  40.18240  116.4142 2020-02-29     1     death
    ## 2363  33.88202  113.6140 2020-02-29     1     death
    ## 2364  30.97560  112.2707 2020-02-29    45     death
    ## 2365  47.60620 -122.3321 2020-02-29     1     death
    ## 2366  47.00000    2.0000 2020-02-29     1 recovered
    ## 2367  32.00000   53.0000 2020-02-29    50 recovered
    ## 2368  36.00000  138.0000 2020-02-29    10 recovered
    ## 2369  21.00000   57.0000 2020-02-29     1 recovered
    ## 2370   1.28330  103.8333 2020-02-29    10 recovered
    ## 2371  36.00000  128.0000 2020-02-29     5 recovered
    ## 2372  31.82570  117.2264 2020-02-29    47 recovered
    ## 2373  40.18240  116.4142 2020-02-29    14 recovered
    ## 2374  30.05720  107.8740 2020-02-29    16 recovered
    ## 2375  26.07890  117.9874 2020-02-29     8 recovered
    ## 2376  23.34170  113.4244 2020-02-29    48 recovered
    ## 2377  23.82980  108.7881 2020-02-29     8 recovered
    ## 2378  19.19590  109.7453 2020-02-29    15 recovered
    ## 2379  38.04280  114.5149 2020-02-29     5 recovered
    ## 2380  47.86200  127.7615 2020-02-29    18 recovered
    ## 2381  33.88202  113.6140 2020-02-29    58 recovered
    ## 2382  22.30000  114.2000 2020-02-29     3 recovered
    ## 2383  30.97560  112.2707 2020-02-29  2590 recovered
    ## 2384  27.61040  111.7088 2020-02-29    16 recovered
    ## 2385  44.09350  113.9448 2020-02-29     4 recovered
    ## 2386  32.97110  119.4550 2020-02-29     8 recovered
    ## 2387  27.61400  115.7221 2020-02-29    21 recovered
    ## 2388  43.66610  126.1923 2020-02-29     2 recovered
    ## 2389  41.29560  122.6085 2020-02-29     3 recovered
    ## 2390  37.26920  106.1655 2020-02-29     1 recovered
    ## 2391  35.19170  108.8701 2020-02-29     8 recovered
    ## 2392  36.34270  118.1498 2020-02-29    16 recovered
    ## 2393  31.20200  121.4491 2020-02-29     8 recovered
    ## 2394  37.57770  112.2922 2020-02-29     2 recovered
    ## 2395  30.61710  102.7103 2020-02-29    13 recovered
    ## 2396  23.70000  121.0000 2020-02-29     3 recovered
    ## 2397  39.30540  117.3230 2020-02-29     7 recovered
    ## 2398  41.11290   85.2401 2020-02-29    10 recovered
    ## 2399  24.97400  101.4870 2020-02-29     1 recovered
    ## 2400  29.18320  120.0934 2020-02-29    41 recovered
    ## 2401  40.06910   45.0382 2020-03-01     1 confirmed
    ## 2402  47.51620   14.5501 2020-03-01     5 confirmed
    ## 2403  40.14310   47.5769 2020-03-01     3 confirmed
    ## 2404  26.02750   50.5500 2020-03-01     6 confirmed
    ## 2405  50.83330    4.0000 2020-03-01     1 confirmed
    ## 2406  45.10000   15.2000 2020-03-01     1 confirmed
    ## 2407  49.81750   15.4730 2020-03-01     3 confirmed
    ## 2408  56.26390    9.5018 2020-03-01     1 confirmed
    ## 2409  18.73570  -70.1627 2020-03-01     1 confirmed
    ## 2410  -1.83120  -78.1834 2020-03-01     6 confirmed
    ## 2411  26.00000   30.0000 2020-03-01     1 confirmed
    ## 2412  64.00000   26.0000 2020-03-01     3 confirmed
    ## 2413  47.00000    2.0000 2020-03-01    30 confirmed
    ## 2414  42.31540   43.3569 2020-03-01     2 confirmed
    ## 2415  51.00000    9.0000 2020-03-01    51 confirmed
    ## 2416  39.07420   21.8243 2020-03-01     3 confirmed
    ## 2417  64.96310  -19.0208 2020-03-01     2 confirmed
    ## 2418  32.00000   53.0000 2020-03-01   385 confirmed
    ## 2419  33.00000   44.0000 2020-03-01     6 confirmed
    ## 2420  31.00000   35.0000 2020-03-01     3 confirmed
    ## 2421  43.00000   12.0000 2020-03-01   566 confirmed
    ## 2422  36.00000  138.0000 2020-03-01    15 confirmed
    ## 2423  33.85470   35.8623 2020-03-01     6 confirmed
    ## 2424   2.50000  112.5000 2020-03-01     4 confirmed
    ## 2425  23.63450 -102.5528 2020-03-01     1 confirmed
    ## 2426  52.13260    5.2913 2020-03-01     4 confirmed
    ## 2427  60.47200    8.4689 2020-03-01     4 confirmed
    ## 2428  25.35480   51.1839 2020-03-01     2 confirmed
    ## 2429   1.28330  103.8333 2020-03-01     4 confirmed
    ## 2430  36.00000  128.0000 2020-03-01   586 confirmed
    ## 2431  40.00000   -4.0000 2020-03-01    39 confirmed
    ## 2432  63.00000   16.0000 2020-03-01     2 confirmed
    ## 2433  46.81820    8.2275 2020-03-01     9 confirmed
    ## 2434  55.00000   -3.0000 2020-03-01    13 confirmed
    ## 2435  40.18240  116.4142 2020-03-01     2 confirmed
    ## 2436  41.73770  -87.6976 2020-03-01     1 confirmed
    ## 2437  22.30000  114.2000 2020-03-01     1 confirmed
    ## 2438  30.97560  112.2707 2020-03-01   570 confirmed
    ## 2439  47.60620 -122.3321 2020-03-01     3 confirmed
    ## 2440  41.29560  122.6085 2020-03-01     1 confirmed
    ## 2441 -33.86880  151.2093 2020-03-01     2 confirmed
    ## 2442  41.82400  -71.4128 2020-03-01     1 confirmed
    ## 2443  36.34270  118.1498 2020-03-01     2 confirmed
    ## 2444  48.03300 -121.8339 2020-03-01     1 confirmed
    ## 2445  23.70000  121.0000 2020-03-01     1 confirmed
    ## 2446  43.65320  -79.3832 2020-03-01     4 confirmed
    ## 2447  32.00000   53.0000 2020-03-01    11     death
    ## 2448  43.00000   12.0000 2020-03-01     5     death
    ## 2449  36.00000  138.0000 2020-03-01     1     death
    ## 2450  36.00000  128.0000 2020-03-01     1     death
    ## 2451  15.00000  101.0000 2020-03-01     1     death
    ## 2452  33.88202  113.6140 2020-03-01     1     death
    ## 2453  30.97560  112.2707 2020-03-01    34     death
    ## 2454 -31.95050  115.8605 2020-03-01     1     death
    ## 2455  32.00000   53.0000 2020-03-01    52 recovered
    ## 2456  43.00000   12.0000 2020-03-01    37 recovered
    ## 2457  36.00000  128.0000 2020-03-01     3 recovered
    ## 2458  31.82570  117.2264 2020-03-01     5 recovered
    ## 2459  40.18240  116.4142 2020-03-01     5 recovered
    ## 2460  30.05720  107.8740 2020-03-01    12 recovered
    ## 2461  26.07890  117.9874 2020-03-01     4 recovered
    ## 2462  36.06110  103.8343 2020-03-01     2 recovered
    ## 2463  23.34170  113.4244 2020-03-01    33 recovered
    ## 2464  23.82980  108.7881 2020-03-01     5 recovered
    ## 2465  19.19590  109.7453 2020-03-01     1 recovered
    ## 2466  38.04280  114.5149 2020-03-01    12 recovered
    ## 2467  47.86200  127.7615 2020-03-01    41 recovered
    ## 2468  33.88202  113.6140 2020-03-01    28 recovered
    ## 2469  22.30000  114.2000 2020-03-01     3 recovered
    ## 2470  30.97560  112.2707 2020-03-01  2543 recovered
    ## 2471  27.61040  111.7088 2020-03-01    20 recovered
    ## 2472  44.09350  113.9448 2020-03-01     3 recovered
    ## 2473  32.97110  119.4550 2020-03-01    13 recovered
    ## 2474  27.61400  115.7221 2020-03-01    20 recovered
    ## 2475  43.66610  126.1923 2020-03-01     3 recovered
    ## 2476  41.29560  122.6085 2020-03-01     7 recovered
    ## 2477  35.19170  108.8701 2020-03-01     1 recovered
    ## 2478  36.34270  118.1498 2020-03-01    22 recovered
    ## 2479  31.20200  121.4491 2020-03-01     3 recovered
    ## 2480  37.57770  112.2922 2020-03-01     2 recovered
    ## 2481  30.61710  102.7103 2020-03-01    14 recovered
    ## 2482  39.30540  117.3230 2020-03-01     2 recovered
    ## 2483  41.11290   85.2401 2020-03-01     2 recovered
    ## 2484  24.97400  101.4870 2020-03-01     6 recovered
    ## 2485  29.18320  120.0934 2020-03-01    30 recovered
    ## 2486  28.03390    1.6596 2020-03-02     2 confirmed
    ## 2487  42.50630    1.5218 2020-03-02     1 confirmed
    ## 2488  47.51620   14.5501 2020-03-02     4 confirmed
    ## 2489  26.02750   50.5500 2020-03-02     2 confirmed
    ## 2490  50.83330    4.0000 2020-03-02     6 confirmed
    ## 2491  47.00000    2.0000 2020-03-02    61 confirmed
    ## 2492  51.00000    9.0000 2020-03-02    29 confirmed
    ## 2493  64.96310  -19.0208 2020-03-02     3 confirmed
    ## 2494  21.00000   78.0000 2020-03-02     2 confirmed
    ## 2495  -0.78930  113.9213 2020-03-02     2 confirmed
    ## 2496  32.00000   53.0000 2020-03-02   523 confirmed
    ## 2497  33.00000   44.0000 2020-03-02     7 confirmed
    ## 2498  43.00000   12.0000 2020-03-02   342 confirmed
    ## 2499  36.00000  138.0000 2020-03-02    18 confirmed
    ## 2500  29.50000   47.7500 2020-03-02    11 confirmed
    ## 2501  56.87960   24.6032 2020-03-02     1 confirmed
    ## 2502  33.85470   35.8623 2020-03-02     3 confirmed
    ## 2503  31.79170   -7.0926 2020-03-02     1 confirmed
    ## 2504  52.13260    5.2913 2020-03-02     8 confirmed
    ## 2505  60.47200    8.4689 2020-03-02     6 confirmed
    ## 2506  39.39990   -8.2245 2020-03-02     2 confirmed
    ## 2507  60.00000   90.0000 2020-03-02     1 confirmed
    ## 2508  43.94240   12.4578 2020-03-02     7 confirmed
    ## 2509  24.00000   45.0000 2020-03-02     1 confirmed
    ## 2510  14.49740  -14.4524 2020-03-02     1 confirmed
    ## 2511   1.28330  103.8333 2020-03-02     2 confirmed
    ## 2512  36.00000  128.0000 2020-03-02   599 confirmed
    ## 2513  40.00000   -4.0000 2020-03-02    36 confirmed
    ## 2514  63.00000   16.0000 2020-03-02     1 confirmed
    ## 2515  46.81820    8.2275 2020-03-02    15 confirmed
    ## 2516  15.00000  101.0000 2020-03-02     1 confirmed
    ## 2517  55.00000   -3.0000 2020-03-02     4 confirmed
    ## 2518  40.18240  116.4142 2020-03-02     1 confirmed
    ## 2519  41.73770  -87.6976 2020-03-02     1 confirmed
    ## 2520  43.90880  -71.8260 2020-03-02     1 confirmed
    ## 2521  23.34170  113.4244 2020-03-02     1 confirmed
    ## 2522  27.99040  -82.3018 2020-03-02     1 confirmed
    ## 2523  22.30000  114.2000 2020-03-02     4 confirmed
    ## 2524  30.97560  112.2707 2020-03-02   196 confirmed
    ## 2525  47.60620 -122.3321 2020-03-02     5 confirmed
    ## 2526  40.71280  -74.0060 2020-03-02     1 confirmed
    ## 2527  37.26920  106.1655 2020-03-02     1 confirmed
    ## 2528  39.09160 -120.8039 2020-03-02     1 confirmed
    ## 2529  41.82400  -71.4128 2020-03-02     1 confirmed
    ## 2530  37.56300 -122.3255 2020-03-02     1 confirmed
    ## 2531  37.35410 -121.9552 2020-03-02     6 confirmed
    ## 2532  27.33640  -82.5307 2020-03-02     1 confirmed
    ## 2533  48.03300 -121.8339 2020-03-02     2 confirmed
    ## 2534  38.57800 -122.9888 2020-03-02     1 confirmed
    ## 2535  23.70000  121.0000 2020-03-02     1 confirmed
    ## 2536 -41.45450  145.9707 2020-03-02     1 confirmed
    ## 2537  43.65320  -79.3832 2020-03-02     3 confirmed
    ## 2538  45.77500 -118.7606 2020-03-02     1 confirmed
    ## 2539  35.44370  139.6380 2020-03-02     1 confirmed
    ## 2540 -37.81360  144.9631 2020-03-02     2 confirmed
    ## 2541  45.54700 -123.1386 2020-03-02     1 confirmed
    ## 2542  29.18320  120.0934 2020-03-02     1 confirmed
    ## 2543  47.00000    2.0000 2020-03-02     1     death
    ## 2544  32.00000   53.0000 2020-03-02    12     death
    ## 2545  43.00000   12.0000 2020-03-02    18     death
    ## 2546  36.00000  128.0000 2020-03-02    11     death
    ## 2547  30.97560  112.2707 2020-03-02    42     death
    ## 2548  47.60620 -122.3321 2020-03-02     4     death
    ## 2549  48.03300 -121.8339 2020-03-02     1     death
    ## 2550  32.00000   53.0000 2020-03-02   116 recovered
    ## 2551  43.00000   12.0000 2020-03-02    66 recovered
    ## 2552   1.28330  103.8333 2020-03-02     6 recovered
    ## 2553  15.00000  101.0000 2020-03-02     3 recovered
    ## 2554  31.82570  117.2264 2020-03-02    44 recovered
    ## 2555  40.18240  116.4142 2020-03-02     6 recovered
    ## 2556  30.05720  107.8740 2020-03-02    19 recovered
    ## 2557  26.07890  117.9874 2020-03-02     8 recovered
    ## 2558  36.06110  103.8343 2020-03-02     1 recovered
    ## 2559  23.34170  113.4244 2020-03-02    43 recovered
    ## 2560  23.82980  108.7881 2020-03-02    11 recovered
    ## 2561  26.81540  106.8748 2020-03-02     2 recovered
    ## 2562  19.19590  109.7453 2020-03-02     2 recovered
    ## 2563  38.04280  114.5149 2020-03-02     2 recovered
    ## 2564  47.86200  127.7615 2020-03-02    14 recovered
    ## 2565  33.88202  113.6140 2020-03-02     7 recovered
    ## 2566  30.97560  112.2707 2020-03-02  2398 recovered
    ## 2567  27.61040  111.7088 2020-03-02    21 recovered
    ## 2568  44.09350  113.9448 2020-03-02     2 recovered
    ## 2569  32.97110  119.4550 2020-03-02     7 recovered
    ## 2570  27.61400  115.7221 2020-03-02    19 recovered
    ## 2571  43.66610  126.1923 2020-03-02     5 recovered
    ## 2572  35.19170  108.8701 2020-03-02     8 recovered
    ## 2573  36.34270  118.1498 2020-03-02    17 recovered
    ## 2574  31.20200  121.4491 2020-03-02     2 recovered
    ## 2575  37.57770  112.2922 2020-03-02     3 recovered
    ## 2576  30.61710  102.7103 2020-03-02    21 recovered
    ## 2577  23.70000  121.0000 2020-03-02     3 recovered
    ## 2578  41.11290   85.2401 2020-03-02     2 recovered
    ## 2579  24.97400  101.4870 2020-03-02     5 recovered
    ## 2580  29.18320  120.0934 2020-03-02    23 recovered
    ## 2581  28.03390    1.6596 2020-03-03     2 confirmed
    ## 2582 -38.41610  -63.6167 2020-03-03     1 confirmed
    ## 2583  47.51620   14.5501 2020-03-03     3 confirmed
    ## 2584  50.83330    4.0000 2020-03-03     5 confirmed
    ## 2585 -35.67510  -71.5430 2020-03-03     1 confirmed
    ## 2586  45.10000   15.2000 2020-03-03     2 confirmed
    ## 2587  49.81750   15.4730 2020-03-03     2 confirmed
    ## 2588  56.26390    9.5018 2020-03-03     2 confirmed
    ## 2589  -1.83120  -78.1834 2020-03-03     1 confirmed
    ## 2590  58.59530   25.0136 2020-03-03     1 confirmed
    ## 2591  47.00000    2.0000 2020-03-03    13 confirmed
    ## 2592  51.00000    9.0000 2020-03-03    37 confirmed
    ## 2593  64.96310  -19.0208 2020-03-03     5 confirmed
    ## 2594  32.00000   53.0000 2020-03-03   835 confirmed
    ## 2595  33.00000   44.0000 2020-03-03     6 confirmed
    ## 2596  53.14240   -7.6921 2020-03-03     1 confirmed
    ## 2597  31.00000   35.0000 2020-03-03     2 confirmed
    ## 2598  43.00000   12.0000 2020-03-03   466 confirmed
    ## 2599  36.00000  138.0000 2020-03-03    19 confirmed
    ## 2600  31.24000   36.5100 2020-03-03     1 confirmed
    ## 2601   2.50000  112.5000 2020-03-03     7 confirmed
    ## 2602  52.13260    5.2913 2020-03-03     6 confirmed
    ## 2603  60.47200    8.4689 2020-03-03     7 confirmed
    ## 2604  21.00000   57.0000 2020-03-03     6 confirmed
    ## 2605  30.37530   69.3451 2020-03-03     1 confirmed
    ## 2606  25.35480   51.1839 2020-03-03     4 confirmed
    ## 2607  43.94240   12.4578 2020-03-03     2 confirmed
    ## 2608  14.49740  -14.4524 2020-03-03     1 confirmed
    ## 2609   1.28330  103.8333 2020-03-03     2 confirmed
    ## 2610  36.00000  128.0000 2020-03-03   851 confirmed
    ## 2611  40.00000   -4.0000 2020-03-03    45 confirmed
    ## 2612  63.00000   16.0000 2020-03-03     6 confirmed
    ## 2613  46.81820    8.2275 2020-03-03    14 confirmed
    ## 2614  55.00000   -3.0000 2020-03-03    11 confirmed
    ## 2615  48.37940   31.1656 2020-03-03     1 confirmed
    ## 2616  24.00000   54.0000 2020-03-03     6 confirmed
    ## 2617  42.17670  -71.1449 2020-03-03     1 confirmed
    ## 2618  37.87150 -122.2730 2020-03-03     1 confirmed
    ## 2619  49.28270 -123.1207 2020-03-03     1 confirmed
    ## 2620  35.44370  139.6380 2020-03-03     1 confirmed
    ## 2621  33.80340  -84.3963 2020-03-03     2 confirmed
    ## 2622  43.90880  -71.8260 2020-03-03     1 confirmed
    ## 2623  27.99040  -82.3018 2020-03-03     1 confirmed
    ## 2624  30.97560  112.2707 2020-03-03   114 confirmed
    ## 2625  47.60620 -122.3321 2020-03-03     7 confirmed
    ## 2626  41.29560  122.6085 2020-03-03     3 confirmed
    ## 2627  33.29180 -112.4291 2020-03-03     1 confirmed
    ## 2628 -33.86880  151.2093 2020-03-03     7 confirmed
    ## 2629 -28.01670  153.4000 2020-03-03     2 confirmed
    ## 2630  37.56300 -122.3255 2020-03-03     1 confirmed
    ## 2631  37.35410 -121.9552 2020-03-03     2 confirmed
    ## 2632  31.20200  121.4491 2020-03-03     1 confirmed
    ## 2633  48.03300 -121.8339 2020-03-03     2 confirmed
    ## 2634  23.70000  121.0000 2020-03-03     1 confirmed
    ## 2635  43.65320  -79.3832 2020-03-03     2 confirmed
    ## 2636  35.80320  -78.5661 2020-03-03     1 confirmed
    ## 2637  41.12200  -73.7949 2020-03-03     1 confirmed
    ## 2638  29.18320  120.0934 2020-03-03     7 confirmed
    ## 2639  47.00000    2.0000 2020-03-03     1     death
    ## 2640  32.00000   53.0000 2020-03-03    11     death
    ## 2641  43.00000   12.0000 2020-03-03    27     death
    ## 2642  43.94240   12.4578 2020-03-03     1     death
    ## 2643  40.00000   -4.0000 2020-03-03     1     death
    ## 2644  30.97560  112.2707 2020-03-03    32     death
    ## 2645  44.09350  113.9448 2020-03-03     1     death
    ## 2646  47.60620 -122.3321 2020-03-03     1     death
    ## 2647  43.00000   12.0000 2020-03-03    11 recovered
    ## 2648  36.00000  138.0000 2020-03-03    11 recovered
    ## 2649   2.50000  112.5000 2020-03-03     4 recovered
    ## 2650  23.63450 -102.5528 2020-03-03     1 recovered
    ## 2651  21.00000   57.0000 2020-03-03     1 recovered
    ## 2652  46.81820    8.2275 2020-03-03     2 recovered
    ## 2653  31.82570  117.2264 2020-03-03    19 recovered
    ## 2654  40.18240  116.4142 2020-03-03     6 recovered
    ## 2655  30.05720  107.8740 2020-03-03    21 recovered
    ## 2656  26.07890  117.9874 2020-03-03     5 recovered
    ## 2657  36.06110  103.8343 2020-03-03     1 recovered
    ## 2658  23.34170  113.4244 2020-03-03    42 recovered
    ## 2659  23.82980  108.7881 2020-03-03    10 recovered
    ## 2660  19.19590  109.7453 2020-03-03     4 recovered
    ## 2661  38.04280  114.5149 2020-03-03     4 recovered
    ## 2662  47.86200  127.7615 2020-03-03    10 recovered
    ## 2663  33.88202  113.6140 2020-03-03    26 recovered
    ## 2664  22.30000  114.2000 2020-03-03     1 recovered
    ## 2665  30.97560  112.2707 2020-03-03  2274 recovered
    ## 2666  27.61040  111.7088 2020-03-03    19 recovered
    ## 2667  44.09350  113.9448 2020-03-03     5 recovered
    ## 2668  32.97110  119.4550 2020-03-03    19 recovered
    ## 2669  27.61400  115.7221 2020-03-03    20 recovered
    ## 2670  41.29560  122.6085 2020-03-03     3 recovered
    ## 2671  22.16670  113.5500 2020-03-03     1 recovered
    ## 2672  43.07310  -89.4012 2020-03-03     1 recovered
    ## 2673  36.34270  118.1498 2020-03-03    51 recovered
    ## 2674  31.20200  121.4491 2020-03-03     2 recovered
    ## 2675  37.57770  112.2922 2020-03-03     5 recovered
    ## 2676  30.61710  102.7103 2020-03-03     8 recovered
    ## 2677  39.30540  117.3230 2020-03-03    13 recovered
    ## 2678  41.11290   85.2401 2020-03-03     2 recovered
    ## 2679  24.97400  101.4870 2020-03-03     1 recovered
    ## 2680  29.18320  120.0934 2020-03-03    24 recovered
    ## 2681  28.03390    1.6596 2020-03-04     7 confirmed
    ## 2682  47.51620   14.5501 2020-03-04     8 confirmed
    ## 2683  26.02750   50.5500 2020-03-04     3 confirmed
    ## 2684  53.70980   27.9534 2020-03-04     5 confirmed
    ## 2685  50.83330    4.0000 2020-03-04    10 confirmed
    ## 2686 -14.23500  -51.9253 2020-03-04     2 confirmed
    ## 2687  45.10000   15.2000 2020-03-04     1 confirmed
    ## 2688  49.81750   15.4730 2020-03-04     3 confirmed
    ## 2689  56.26390    9.5018 2020-03-04     4 confirmed
    ## 2690  -1.83120  -78.1834 2020-03-04     3 confirmed
    ## 2691  61.89260   -6.9118 2020-03-04     1 confirmed
    ## 2692  47.00000    2.0000 2020-03-04    81 confirmed
    ## 2693  51.00000    9.0000 2020-03-04    66 confirmed
    ## 2694  36.14080   -5.3536 2020-03-04     1 confirmed
    ## 2695  39.07420   21.8243 2020-03-04     2 confirmed
    ## 2696  47.16250   19.5033 2020-03-04     2 confirmed
    ## 2697  64.96310  -19.0208 2020-03-04    15 confirmed
    ## 2698  21.00000   78.0000 2020-03-04    23 confirmed
    ## 2699  32.00000   53.0000 2020-03-04   586 confirmed
    ## 2700  33.00000   44.0000 2020-03-04     3 confirmed
    ## 2701  53.14240   -7.6921 2020-03-04     4 confirmed
    ## 2702  31.00000   35.0000 2020-03-04     3 confirmed
    ## 2703  43.00000   12.0000 2020-03-04   587 confirmed
    ## 2704  36.00000  138.0000 2020-03-04    38 confirmed
    ## 2705  47.14000    9.5500 2020-03-04     1 confirmed
    ## 2706   2.50000  112.5000 2020-03-04    14 confirmed
    ## 2707  52.13260    5.2913 2020-03-04    14 confirmed
    ## 2708 -40.90060  174.8860 2020-03-04     2 confirmed
    ## 2709  60.47200    8.4689 2020-03-04    24 confirmed
    ## 2710  21.00000   57.0000 2020-03-04     3 confirmed
    ## 2711  51.91940   19.1451 2020-03-04     1 confirmed
    ## 2712  39.39990   -8.2245 2020-03-04     3 confirmed
    ## 2713  25.35480   51.1839 2020-03-04     1 confirmed
    ## 2714  45.94320   24.9668 2020-03-04     1 confirmed
    ## 2715  17.90000  -62.8333 2020-03-04     3 confirmed
    ## 2716  43.94240   12.4578 2020-03-04     6 confirmed
    ## 2717  14.49740  -14.4524 2020-03-04     2 confirmed
    ## 2718  36.00000  128.0000 2020-03-04   435 confirmed
    ## 2719  40.00000   -4.0000 2020-03-04    57 confirmed
    ## 2720  63.00000   16.0000 2020-03-04    14 confirmed
    ## 2721  46.81820    8.2275 2020-03-04    34 confirmed
    ## 2722  34.00000    9.0000 2020-03-04     1 confirmed
    ## 2723  55.00000   -3.0000 2020-03-04    34 confirmed
    ## 2724  40.18240  116.4142 2020-03-04     4 confirmed
    ## 2725  49.28270 -123.1207 2020-03-04     3 confirmed
    ## 2726  37.85340 -121.9018 2020-03-04     1 confirmed
    ## 2727  22.30000  114.2000 2020-03-04     5 confirmed
    ## 2728  30.97560  112.2707 2020-03-04   115 confirmed
    ## 2729  47.60620 -122.3321 2020-03-04    10 confirmed
    ## 2730  34.05220 -118.2437 2020-03-04     6 confirmed
    ## 2731 -33.86880  151.2093 2020-03-04     9 confirmed
    ## 2732  37.26920  106.1655 2020-03-04     1 confirmed
    ## 2733 -12.46340  130.8456 2020-03-04     1 confirmed
    ## 2734  33.78790 -117.8531 2020-03-04     2 confirmed
    ## 2735  39.09160 -120.8039 2020-03-04     1 confirmed
    ## 2736  48.03300 -121.8339 2020-03-04     2 confirmed
    ## 2737 -34.92850  138.6007 2020-03-04     2 confirmed
    ## 2738 -37.81360  144.9631 2020-03-04     1 confirmed
    ## 2739  41.12200  -73.7949 2020-03-04     9 confirmed
    ## 2740  32.00000   53.0000 2020-03-04    15     death
    ## 2741  33.00000   44.0000 2020-03-04     2     death
    ## 2742  43.00000   12.0000 2020-03-04    28     death
    ## 2743  36.00000  128.0000 2020-03-04     7     death
    ## 2744  40.00000   -4.0000 2020-03-04     1     death
    ## 2745  30.97560  112.2707 2020-03-04    36     death
    ## 2746  47.60620 -122.3321 2020-03-04     3     death
    ## 2747 -33.86880  151.2093 2020-03-04     1     death
    ## 2748  39.09160 -120.8039 2020-03-04     1     death
    ## 2749  32.00000   53.0000 2020-03-04   261 recovered
    ## 2750  43.00000   12.0000 2020-03-04   116 recovered
    ## 2751  33.85470   35.8623 2020-03-04     1 recovered
    ## 2752  45.94320   24.9668 2020-03-04     1 recovered
    ## 2753  36.00000  128.0000 2020-03-04    11 recovered
    ## 2754  46.81820    8.2275 2020-03-04     1 recovered
    ## 2755  31.82570  117.2264 2020-03-04    20 recovered
    ## 2756  40.18240  116.4142 2020-03-04     9 recovered
    ## 2757  30.05720  107.8740 2020-03-04    12 recovered
    ## 2758  26.07890  117.9874 2020-03-04    10 recovered
    ## 2759  36.06110  103.8343 2020-03-04     1 recovered
    ## 2760  23.34170  113.4244 2020-03-04    32 recovered
    ## 2761  23.82980  108.7881 2020-03-04     8 recovered
    ## 2762  19.19590  109.7453 2020-03-04     3 recovered
    ## 2763  38.04280  114.5149 2020-03-04     1 recovered
    ## 2764  47.86200  127.7615 2020-03-04     7 recovered
    ## 2765  33.88202  113.6140 2020-03-04     3 recovered
    ## 2766  30.97560  112.2707 2020-03-04  2349 recovered
    ## 2767  27.61040  111.7088 2020-03-04    10 recovered
    ## 2768  44.09350  113.9448 2020-03-04     4 recovered
    ## 2769  32.97110  119.4550 2020-03-04    15 recovered
    ## 2770  27.61400  115.7221 2020-03-04    14 recovered
    ## 2771  43.66610  126.1923 2020-03-04     3 recovered
    ## 2772  35.19170  108.8701 2020-03-04     7 recovered
    ## 2773  36.34270  118.1498 2020-03-04     5 recovered
    ## 2774  31.20200  121.4491 2020-03-04     4 recovered
    ## 2775  30.61710  102.7103 2020-03-04    12 recovered
    ## 2776  41.11290   85.2401 2020-03-04     1 recovered
    ## 2777  29.18320  120.0934 2020-03-04    21 recovered
    ## 2778  47.51620   14.5501 2020-03-05    12 confirmed
    ## 2779  40.14310   47.5769 2020-03-05     3 confirmed
    ## 2780  26.02750   50.5500 2020-03-05     3 confirmed
    ## 2781  50.83330    4.0000 2020-03-05    27 confirmed
    ## 2782  43.91590   17.6791 2020-03-05     2 confirmed
    ## 2783 -35.67510  -71.5430 2020-03-05     3 confirmed
    ## 2784  49.81750   15.4730 2020-03-05     4 confirmed
    ## 2785  -1.83120  -78.1834 2020-03-05     3 confirmed
    ## 2786  26.00000   30.0000 2020-03-05     1 confirmed
    ## 2787  58.59530   25.0136 2020-03-05     1 confirmed
    ## 2788  64.00000   26.0000 2020-03-05     6 confirmed
    ## 2789  47.00000    2.0000 2020-03-05    92 confirmed
    ## 2790  42.31540   43.3569 2020-03-05     1 confirmed
    ## 2791  51.00000    9.0000 2020-03-05   220 confirmed
    ## 2792  39.07420   21.8243 2020-03-05    22 confirmed
    ## 2793  64.96310  -19.0208 2020-03-05     8 confirmed
    ## 2794  21.00000   78.0000 2020-03-05     2 confirmed
    ## 2795  32.00000   53.0000 2020-03-05   591 confirmed
    ## 2796  31.00000   35.0000 2020-03-05     1 confirmed
    ## 2797  43.00000   12.0000 2020-03-05   769 confirmed
    ## 2798  36.00000  138.0000 2020-03-05    29 confirmed
    ## 2799  29.50000   47.7500 2020-03-05     2 confirmed
    ## 2800  33.85470   35.8623 2020-03-05     3 confirmed
    ## 2801  31.79170   -7.0926 2020-03-05     1 confirmed
    ## 2802  52.13260    5.2913 2020-03-05    44 confirmed
    ## 2803  60.47200    8.4689 2020-03-05    31 confirmed
    ## 2804  21.00000   57.0000 2020-03-05     1 confirmed
    ## 2805  31.95220   35.2332 2020-03-05     4 confirmed
    ## 2806  39.39990   -8.2245 2020-03-05     3 confirmed
    ## 2807  45.94320   24.9668 2020-03-05     2 confirmed
    ## 2808  60.00000   90.0000 2020-03-05     1 confirmed
    ## 2809  43.94240   12.4578 2020-03-05     5 confirmed
    ## 2810  24.00000   45.0000 2020-03-05     4 confirmed
    ## 2811   1.28330  103.8333 2020-03-05     7 confirmed
    ## 2812  46.15120   14.9955 2020-03-05     2 confirmed
    ## 2813 -30.55950   22.9375 2020-03-05     1 confirmed
    ## 2814  36.00000  128.0000 2020-03-05   467 confirmed
    ## 2815  40.00000   -4.0000 2020-03-05    37 confirmed
    ## 2816  63.00000   16.0000 2020-03-05    59 confirmed
    ## 2817  46.81820    8.2275 2020-03-05    24 confirmed
    ## 2818  15.00000  101.0000 2020-03-05     4 confirmed
    ## 2819  55.00000   -3.0000 2020-03-05    30 confirmed
    ## 2820  24.00000   54.0000 2020-03-05     2 confirmed
    ## 2821  45.50170  -73.5673 2020-03-05     1 confirmed
    ## 2822  40.92630  -74.0770 2020-03-05     2 confirmed
    ## 2823  49.28270 -123.1207 2020-03-05     1 confirmed
    ## 2824  36.07960 -115.0940 2020-03-05     1 confirmed
    ## 2825  41.73770  -87.6976 2020-03-05     1 confirmed
    ## 2826  29.56930  -95.8143 2020-03-05     1 confirmed
    ## 2827  36.06110  103.8343 2020-03-05    11 confirmed
    ## 2828  47.19810 -119.3732 2020-03-05     1 confirmed
    ## 2829  23.34170  113.4244 2020-03-05     1 confirmed
    ## 2830  29.77520  -95.3103 2020-03-05     2 confirmed
    ## 2831  47.86200  127.7615 2020-03-05     1 confirmed
    ## 2832  30.97560  112.2707 2020-03-05   134 confirmed
    ## 2833  47.60620 -122.3321 2020-03-05    20 confirmed
    ## 2834  34.05220 -118.2437 2020-03-05     4 confirmed
    ## 2835  40.71280  -74.0060 2020-03-05     3 confirmed
    ## 2836  40.72820  -73.7949 2020-03-05     1 confirmed
    ## 2837 -28.01670  153.4000 2020-03-05     2 confirmed
    ## 2838  32.71570 -117.1611 2020-03-05     1 confirmed
    ## 2839  37.77490 -122.4194 2020-03-05     2 confirmed
    ## 2840  37.35410 -121.9552 2020-03-05     9 confirmed
    ## 2841  30.76900  -86.9824 2020-03-05     1 confirmed
    ## 2842  31.20200  121.4491 2020-03-05     1 confirmed
    ## 2843  30.61710  102.7103 2020-03-05     1 confirmed
    ## 2844  48.03300 -121.8339 2020-03-05    10 confirmed
    ## 2845  23.70000  121.0000 2020-03-05     2 confirmed
    ## 2846  43.65320  -79.3832 2020-03-05     2 confirmed
    ## 2847  41.12200  -73.7949 2020-03-05     8 confirmed
    ## 2848 -31.95050  115.8605 2020-03-05     1 confirmed
    ## 2849  35.91790  -86.8622 2020-03-05     1 confirmed
    ## 2850  29.18320  120.0934 2020-03-05     2 confirmed
    ## 2851  47.00000    2.0000 2020-03-05     2     death
    ## 2852  32.00000   53.0000 2020-03-05    15     death
    ## 2853  43.00000   12.0000 2020-03-05    41     death
    ## 2854  40.00000   -4.0000 2020-03-05     1     death
    ## 2855  46.81820    8.2275 2020-03-05     1     death
    ## 2856  55.00000   -3.0000 2020-03-05     1     death
    ## 2857  19.19590  109.7453 2020-03-05     1     death
    ## 2858  30.97560  112.2707 2020-03-05    31     death
    ## 2859  47.60620 -122.3321 2020-03-05     1     death
    ## 2860  32.00000   53.0000 2020-03-05   187 recovered
    ## 2861  43.00000   12.0000 2020-03-05   138 recovered
    ## 2862  31.82570  117.2264 2020-03-05    14 recovered
    ## 2863  30.05720  107.8740 2020-03-05    10 recovered
    ## 2864  26.07890  117.9874 2020-03-05     7 recovered
    ## 2865  23.34170  113.4244 2020-03-05    48 recovered
    ## 2866  23.82980  108.7881 2020-03-05     4 recovered
    ## 2867  38.04280  114.5149 2020-03-05     3 recovered
    ## 2868  47.86200  127.7615 2020-03-05     6 recovered
    ## 2869  33.88202  113.6140 2020-03-05     5 recovered
    ## 2870  22.30000  114.2000 2020-03-05     6 recovered
    ## 2871  30.97560  112.2707 2020-03-05  2035 recovered
    ## 2872  27.61040  111.7088 2020-03-05    22 recovered
    ## 2873  44.09350  113.9448 2020-03-05     2 recovered
    ## 2874  32.97110  119.4550 2020-03-05     6 recovered
    ## 2875  27.61400  115.7221 2020-03-05    17 recovered
    ## 2876  43.66610  126.1923 2020-03-05     2 recovered
    ## 2877 -28.01670  153.4000 2020-03-05     7 recovered
    ## 2878  35.19170  108.8701 2020-03-05     1 recovered
    ## 2879  36.34270  118.1498 2020-03-05    62 recovered
    ## 2880  31.20200  121.4491 2020-03-05     5 recovered
    ## 2881  37.57770  112.2922 2020-03-05     2 recovered
    ## 2882  30.61710  102.7103 2020-03-05    19 recovered
    ## 2883  39.30540  117.3230 2020-03-05     4 recovered
    ## 2884 -37.81360  144.9631 2020-03-05     3 recovered
    ## 2885  41.11290   85.2401 2020-03-05     1 recovered
    ## 2886  29.18320  120.0934 2020-03-05    10 recovered

\#\#Time for some HW\!

``` r
mainland <- virus[grep("Mainland China",virus$Country.Region),]
mainland
```

    ##      Province.State Country.Region      Lat     Long       date cases      type
    ## 4             Anhui Mainland China 31.82570 117.2264 2020-01-22     1 confirmed
    ## 5           Beijing Mainland China 40.18240 116.4142 2020-01-22    14 confirmed
    ## 6         Chongqing Mainland China 30.05720 107.8740 2020-01-22     6 confirmed
    ## 7            Fujian Mainland China 26.07890 117.9874 2020-01-22     1 confirmed
    ## 8         Guangdong Mainland China 23.34170 113.4244 2020-01-22    26 confirmed
    ## 9           Guangxi Mainland China 23.82980 108.7881 2020-01-22     2 confirmed
    ## 10          Guizhou Mainland China 26.81540 106.8748 2020-01-22     1 confirmed
    ## 11           Hainan Mainland China 19.19590 109.7453 2020-01-22     4 confirmed
    ## 12            Hebei Mainland China 38.04280 114.5149 2020-01-22     1 confirmed
    ## 13            Henan Mainland China 33.88202 113.6140 2020-01-22     5 confirmed
    ## 14            Hubei Mainland China 30.97560 112.2707 2020-01-22   444 confirmed
    ## 15            Hunan Mainland China 27.61040 111.7088 2020-01-22     4 confirmed
    ## 16          Jiangsu Mainland China 32.97110 119.4550 2020-01-22     1 confirmed
    ## 17          Jiangxi Mainland China 27.61400 115.7221 2020-01-22     2 confirmed
    ## 19         Liaoning Mainland China 41.29560 122.6085 2020-01-22     2 confirmed
    ## 21          Ningxia Mainland China 37.26920 106.1655 2020-01-22     1 confirmed
    ## 22         Shandong Mainland China 36.34270 118.1498 2020-01-22     2 confirmed
    ## 23         Shanghai Mainland China 31.20200 121.4491 2020-01-22     9 confirmed
    ## 24           Shanxi Mainland China 37.57770 112.2922 2020-01-22     1 confirmed
    ## 25          Sichuan Mainland China 30.61710 102.7103 2020-01-22     5 confirmed
    ## 27          Tianjin Mainland China 39.30540 117.3230 2020-01-22     4 confirmed
    ## 28           Yunnan Mainland China 24.97400 101.4870 2020-01-22     1 confirmed
    ## 29         Zhejiang Mainland China 29.18320 120.0934 2020-01-22    10 confirmed
    ## 30            Hubei Mainland China 30.97560 112.2707 2020-01-22    17     death
    ## 31            Hubei Mainland China 30.97560 112.2707 2020-01-22    28 recovered
    ## 36            Anhui Mainland China 31.82570 117.2264 2020-01-23     8 confirmed
    ## 37          Beijing Mainland China 40.18240 116.4142 2020-01-23     8 confirmed
    ## 38        Chongqing Mainland China 30.05720 107.8740 2020-01-23     3 confirmed
    ## 39           Fujian Mainland China 26.07890 117.9874 2020-01-23     4 confirmed
    ## 40            Gansu Mainland China 36.06110 103.8343 2020-01-23     2 confirmed
    ## 41        Guangdong Mainland China 23.34170 113.4244 2020-01-23     6 confirmed
    ## 42          Guangxi Mainland China 23.82980 108.7881 2020-01-23     3 confirmed
    ## 43          Guizhou Mainland China 26.81540 106.8748 2020-01-23     2 confirmed
    ## 44           Hainan Mainland China 19.19590 109.7453 2020-01-23     1 confirmed
    ## 45     Heilongjiang Mainland China 47.86200 127.7615 2020-01-23     2 confirmed
    ## 47            Hunan Mainland China 27.61040 111.7088 2020-01-23     5 confirmed
    ## 48          Jiangsu Mainland China 32.97110 119.4550 2020-01-23     4 confirmed
    ## 49          Jiangxi Mainland China 27.61400 115.7221 2020-01-23     5 confirmed
    ## 50            Jilin Mainland China 43.66610 126.1923 2020-01-23     1 confirmed
    ## 51         Liaoning Mainland China 41.29560 122.6085 2020-01-23     1 confirmed
    ## 53          Shaanxi Mainland China 35.19170 108.8701 2020-01-23     3 confirmed
    ## 54         Shandong Mainland China 36.34270 118.1498 2020-01-23     4 confirmed
    ## 55         Shanghai Mainland China 31.20200 121.4491 2020-01-23     7 confirmed
    ## 56          Sichuan Mainland China 30.61710 102.7103 2020-01-23     3 confirmed
    ## 57         Xinjiang Mainland China 41.11290  85.2401 2020-01-23     2 confirmed
    ## 58           Yunnan Mainland China 24.97400 101.4870 2020-01-23     1 confirmed
    ## 59         Zhejiang Mainland China 29.18320 120.0934 2020-01-23    17 confirmed
    ## 60            Hebei Mainland China 38.04280 114.5149 2020-01-23     1     death
    ## 61        Guangdong Mainland China 23.34170 113.4244 2020-01-23     2 recovered
    ## 67            Anhui Mainland China 31.82570 117.2264 2020-01-24     6 confirmed
    ## 68          Beijing Mainland China 40.18240 116.4142 2020-01-24    14 confirmed
    ## 69        Chongqing Mainland China 30.05720 107.8740 2020-01-24    18 confirmed
    ## 71           Fujian Mainland China 26.07890 117.9874 2020-01-24     5 confirmed
    ## 72        Guangdong Mainland China 23.34170 113.4244 2020-01-24    21 confirmed
    ## 73          Guangxi Mainland China 23.82980 108.7881 2020-01-24    18 confirmed
    ## 74           Hainan Mainland China 19.19590 109.7453 2020-01-24     3 confirmed
    ## 75            Hebei Mainland China 38.04280 114.5149 2020-01-24     1 confirmed
    ## 76     Heilongjiang Mainland China 47.86200 127.7615 2020-01-24     2 confirmed
    ## 77            Henan Mainland China 33.88202 113.6140 2020-01-24     4 confirmed
    ## 78            Hubei Mainland China 30.97560 112.2707 2020-01-24   105 confirmed
    ## 79            Hunan Mainland China 27.61040 111.7088 2020-01-24    15 confirmed
    ## 80   Inner Mongolia Mainland China 44.09350 113.9448 2020-01-24     1 confirmed
    ## 81          Jiangsu Mainland China 32.97110 119.4550 2020-01-24     4 confirmed
    ## 82          Jiangxi Mainland China 27.61400 115.7221 2020-01-24    11 confirmed
    ## 83            Jilin Mainland China 43.66610 126.1923 2020-01-24     2 confirmed
    ## 84         Liaoning Mainland China 41.29560 122.6085 2020-01-24     1 confirmed
    ## 85          Ningxia Mainland China 37.26920 106.1655 2020-01-24     1 confirmed
    ## 86          Shaanxi Mainland China 35.19170 108.8701 2020-01-24     2 confirmed
    ## 87         Shandong Mainland China 36.34270 118.1498 2020-01-24     9 confirmed
    ## 88         Shanghai Mainland China 31.20200 121.4491 2020-01-24     4 confirmed
    ## 89          Sichuan Mainland China 30.61710 102.7103 2020-01-24     7 confirmed
    ## 91          Tianjin Mainland China 39.30540 117.3230 2020-01-24     4 confirmed
    ## 92           Yunnan Mainland China 24.97400 101.4870 2020-01-24     3 confirmed
    ## 93         Zhejiang Mainland China 29.18320 120.0934 2020-01-24    16 confirmed
    ## 94     Heilongjiang Mainland China 47.86200 127.7615 2020-01-24     1     death
    ## 95            Hubei Mainland China 30.97560 112.2707 2020-01-24     7     death
    ## 96          Beijing Mainland China 40.18240 116.4142 2020-01-24     1 recovered
    ## 97            Hubei Mainland China 30.97560 112.2707 2020-01-24     3 recovered
    ## 98         Shanghai Mainland China 31.20200 121.4491 2020-01-24     1 recovered
    ## 99         Zhejiang Mainland China 29.18320 120.0934 2020-01-24     1 recovered
    ## 104           Anhui Mainland China 31.82570 117.2264 2020-01-25    24 confirmed
    ## 105         Beijing Mainland China 40.18240 116.4142 2020-01-25     5 confirmed
    ## 106       Chongqing Mainland China 30.05720 107.8740 2020-01-25    30 confirmed
    ## 107          Fujian Mainland China 26.07890 117.9874 2020-01-25     8 confirmed
    ## 108           Gansu Mainland China 36.06110 103.8343 2020-01-25     2 confirmed
    ## 109       Guangdong Mainland China 23.34170 113.4244 2020-01-25    25 confirmed
    ## 110         Guizhou Mainland China 26.81540 106.8748 2020-01-25     1 confirmed
    ## 111          Hainan Mainland China 19.19590 109.7453 2020-01-25    11 confirmed
    ## 112           Hebei Mainland China 38.04280 114.5149 2020-01-25     6 confirmed
    ## 113    Heilongjiang Mainland China 47.86200 127.7615 2020-01-25     5 confirmed
    ## 114           Henan Mainland China 33.88202 113.6140 2020-01-25    23 confirmed
    ## 116           Hubei Mainland China 30.97560 112.2707 2020-01-25   212 confirmed
    ## 117           Hunan Mainland China 27.61040 111.7088 2020-01-25    19 confirmed
    ## 118  Inner Mongolia Mainland China 44.09350 113.9448 2020-01-25     6 confirmed
    ## 119         Jiangsu Mainland China 32.97110 119.4550 2020-01-25     9 confirmed
    ## 120           Jilin Mainland China 43.66610 126.1923 2020-01-25     1 confirmed
    ## 121        Liaoning Mainland China 41.29560 122.6085 2020-01-25    13 confirmed
    ## 122         Ningxia Mainland China 37.26920 106.1655 2020-01-25     1 confirmed
    ## 123         Qinghai Mainland China 35.74520  95.9956 2020-01-25     1 confirmed
    ## 124         Shaanxi Mainland China 35.19170 108.8701 2020-01-25    10 confirmed
    ## 125        Shandong Mainland China 36.34270 118.1498 2020-01-25    12 confirmed
    ## 126        Shanghai Mainland China 31.20200 121.4491 2020-01-25    13 confirmed
    ## 127          Shanxi Mainland China 37.57770 112.2922 2020-01-25     5 confirmed
    ## 128         Sichuan Mainland China 30.61710 102.7103 2020-01-25    13 confirmed
    ## 129         Tianjin Mainland China 39.30540 117.3230 2020-01-25     2 confirmed
    ## 130        Xinjiang Mainland China 41.11290  85.2401 2020-01-25     1 confirmed
    ## 131          Yunnan Mainland China 24.97400 101.4870 2020-01-25     6 confirmed
    ## 132        Zhejiang Mainland China 29.18320 120.0934 2020-01-25    19 confirmed
    ## 133           Hubei Mainland China 30.97560 112.2707 2020-01-25    16     death
    ## 134         Beijing Mainland China 40.18240 116.4142 2020-01-25     1 recovered
    ## 135           Hubei Mainland China 30.97560 112.2707 2020-01-25     1 recovered
    ## 136         Jiangsu Mainland China 32.97110 119.4550 2020-01-25     1 recovered
    ## 142           Anhui Mainland China 31.82570 117.2264 2020-01-26    21 confirmed
    ## 143         Beijing Mainland China 40.18240 116.4142 2020-01-26    27 confirmed
    ## 144       Chongqing Mainland China 30.05720 107.8740 2020-01-26    18 confirmed
    ## 145          Fujian Mainland China 26.07890 117.9874 2020-01-26    17 confirmed
    ## 146           Gansu Mainland China 36.06110 103.8343 2020-01-26     3 confirmed
    ## 147       Guangdong Mainland China 23.34170 113.4244 2020-01-26    33 confirmed
    ## 148         Guangxi Mainland China 23.82980 108.7881 2020-01-26    13 confirmed
    ## 149         Guizhou Mainland China 26.81540 106.8748 2020-01-26     1 confirmed
    ## 150          Hainan Mainland China 19.19590 109.7453 2020-01-26     3 confirmed
    ## 151           Hebei Mainland China 38.04280 114.5149 2020-01-26     5 confirmed
    ## 152    Heilongjiang Mainland China 47.86200 127.7615 2020-01-26     6 confirmed
    ## 153           Henan Mainland China 33.88202 113.6140 2020-01-26    51 confirmed
    ## 155           Hubei Mainland China 30.97560 112.2707 2020-01-26   297 confirmed
    ## 156           Hunan Mainland China 27.61040 111.7088 2020-01-26    26 confirmed
    ## 157         Jiangsu Mainland China 32.97110 119.4550 2020-01-26    15 confirmed
    ## 158         Jiangxi Mainland China 27.61400 115.7221 2020-01-26    18 confirmed
    ## 159        Liaoning Mainland China 41.29560 122.6085 2020-01-26     4 confirmed
    ## 163         Ningxia Mainland China 37.26920 106.1655 2020-01-26     1 confirmed
    ## 165         Shaanxi Mainland China 35.19170 108.8701 2020-01-26     7 confirmed
    ## 166        Shandong Mainland China 36.34270 118.1498 2020-01-26    19 confirmed
    ## 167        Shanghai Mainland China 31.20200 121.4491 2020-01-26     7 confirmed
    ## 168          Shanxi Mainland China 37.57770 112.2922 2020-01-26     3 confirmed
    ## 169         Sichuan Mainland China 30.61710 102.7103 2020-01-26    16 confirmed
    ## 172         Tianjin Mainland China 39.30540 117.3230 2020-01-26     4 confirmed
    ## 175        Xinjiang Mainland China 41.11290  85.2401 2020-01-26     1 confirmed
    ## 176          Yunnan Mainland China 24.97400 101.4870 2020-01-26     5 confirmed
    ## 177        Zhejiang Mainland China 29.18320 120.0934 2020-01-26    42 confirmed
    ## 178           Henan Mainland China 33.88202 113.6140 2020-01-26     1     death
    ## 179           Hubei Mainland China 30.97560 112.2707 2020-01-26    12     death
    ## 180        Shanghai Mainland China 31.20200 121.4491 2020-01-26     1     death
    ## 183           Hubei Mainland China 30.97560 112.2707 2020-01-26    10 recovered
    ## 189           Anhui Mainland China 31.82570 117.2264 2020-01-27    10 confirmed
    ## 190         Beijing Mainland China 40.18240 116.4142 2020-01-27    12 confirmed
    ## 191       Chongqing Mainland China 30.05720 107.8740 2020-01-27    35 confirmed
    ## 192          Fujian Mainland China 26.07890 117.9874 2020-01-27    24 confirmed
    ## 193           Gansu Mainland China 36.06110 103.8343 2020-01-27     7 confirmed
    ## 194       Guangdong Mainland China 23.34170 113.4244 2020-01-27    40 confirmed
    ## 195         Guangxi Mainland China 23.82980 108.7881 2020-01-27    10 confirmed
    ## 196         Guizhou Mainland China 26.81540 106.8748 2020-01-27     2 confirmed
    ## 197          Hainan Mainland China 19.19590 109.7453 2020-01-27    11 confirmed
    ## 198           Hebei Mainland China 38.04280 114.5149 2020-01-27     5 confirmed
    ## 199    Heilongjiang Mainland China 47.86200 127.7615 2020-01-27     6 confirmed
    ## 200           Henan Mainland China 33.88202 113.6140 2020-01-27    45 confirmed
    ## 201           Hubei Mainland China 30.97560 112.2707 2020-01-27   365 confirmed
    ## 202           Hunan Mainland China 27.61040 111.7088 2020-01-27    31 confirmed
    ## 203  Inner Mongolia Mainland China 44.09350 113.9448 2020-01-27     4 confirmed
    ## 204         Jiangsu Mainland China 32.97110 119.4550 2020-01-27    14 confirmed
    ## 205         Jiangxi Mainland China 27.61400 115.7221 2020-01-27    36 confirmed
    ## 206           Jilin Mainland China 43.66610 126.1923 2020-01-27     2 confirmed
    ## 207        Liaoning Mainland China 41.29560 122.6085 2020-01-27     6 confirmed
    ## 210         Ningxia Mainland China 37.26920 106.1655 2020-01-27     3 confirmed
    ## 211         Qinghai Mainland China 35.74520  95.9956 2020-01-27     5 confirmed
    ## 212         Shaanxi Mainland China 35.19170 108.8701 2020-01-27    13 confirmed
    ## 213        Shandong Mainland China 36.34270 118.1498 2020-01-27    29 confirmed
    ## 214        Shanghai Mainland China 31.20200 121.4491 2020-01-27    13 confirmed
    ## 215          Shanxi Mainland China 37.57770 112.2922 2020-01-27     4 confirmed
    ## 216         Sichuan Mainland China 30.61710 102.7103 2020-01-27    25 confirmed
    ## 218         Tianjin Mainland China 39.30540 117.3230 2020-01-27     9 confirmed
    ## 219        Xinjiang Mainland China 41.11290  85.2401 2020-01-27     1 confirmed
    ## 220          Yunnan Mainland China 24.97400 101.4870 2020-01-27    10 confirmed
    ## 221        Zhejiang Mainland China 29.18320 120.0934 2020-01-27    24 confirmed
    ## 222         Beijing Mainland China 40.18240 116.4142 2020-01-27     1     death
    ## 223          Hainan Mainland China 19.19590 109.7453 2020-01-27     1     death
    ## 224           Hubei Mainland China 30.97560 112.2707 2020-01-27    24     death
    ## 225       Guangdong Mainland China 23.34170 113.4244 2020-01-27     2 recovered
    ## 226           Hubei Mainland China 30.97560 112.2707 2020-01-27     3 recovered
    ## 227         Jiangxi Mainland China 27.61400 115.7221 2020-01-27     2 recovered
    ## 228        Shanghai Mainland China 31.20200 121.4491 2020-01-27     2 recovered
    ## 234           Anhui Mainland China 31.82570 117.2264 2020-01-28    36 confirmed
    ## 235         Beijing Mainland China 40.18240 116.4142 2020-01-28    11 confirmed
    ## 237       Chongqing Mainland China 30.05720 107.8740 2020-01-28    22 confirmed
    ## 238          Fujian Mainland China 26.07890 117.9874 2020-01-28    21 confirmed
    ## 239           Gansu Mainland China 36.06110 103.8343 2020-01-28     5 confirmed
    ## 240       Guangdong Mainland China 23.34170 113.4244 2020-01-28    56 confirmed
    ## 241         Guangxi Mainland China 23.82980 108.7881 2020-01-28     5 confirmed
    ## 242         Guizhou Mainland China 26.81540 106.8748 2020-01-28     2 confirmed
    ## 243          Hainan Mainland China 19.19590 109.7453 2020-01-28     7 confirmed
    ## 244           Hebei Mainland China 38.04280 114.5149 2020-01-28    15 confirmed
    ## 245    Heilongjiang Mainland China 47.86200 127.7615 2020-01-28    12 confirmed
    ## 246           Henan Mainland China 33.88202 113.6140 2020-01-28    40 confirmed
    ## 247           Hubei Mainland China 30.97560 112.2707 2020-01-28  2131 confirmed
    ## 248           Hunan Mainland China 27.61040 111.7088 2020-01-28    43 confirmed
    ## 249  Inner Mongolia Mainland China 44.09350 113.9448 2020-01-28     4 confirmed
    ## 250         Jiangsu Mainland China 32.97110 119.4550 2020-01-28    23 confirmed
    ## 251         Jiangxi Mainland China 27.61400 115.7221 2020-01-28    37 confirmed
    ## 252           Jilin Mainland China 43.66610 126.1923 2020-01-28     2 confirmed
    ## 253        Liaoning Mainland China 41.29560 122.6085 2020-01-28     7 confirmed
    ## 255         Ningxia Mainland China 37.26920 106.1655 2020-01-28     4 confirmed
    ## 256         Shaanxi Mainland China 35.19170 108.8701 2020-01-28    11 confirmed
    ## 257        Shandong Mainland China 36.34270 118.1498 2020-01-28    20 confirmed
    ## 258        Shanghai Mainland China 31.20200 121.4491 2020-01-28    13 confirmed
    ## 259          Shanxi Mainland China 37.57770 112.2922 2020-01-28    14 confirmed
    ## 260         Sichuan Mainland China 30.61710 102.7103 2020-01-28    21 confirmed
    ## 262         Tianjin Mainland China 39.30540 117.3230 2020-01-28     1 confirmed
    ## 263        Xinjiang Mainland China 41.11290  85.2401 2020-01-28     5 confirmed
    ## 264          Yunnan Mainland China 24.97400 101.4870 2020-01-28    18 confirmed
    ## 265        Zhejiang Mainland China 29.18320 120.0934 2020-01-28    45 confirmed
    ## 266           Hubei Mainland China 30.97560 112.2707 2020-01-28    49     death
    ## 268         Beijing Mainland China 40.18240 116.4142 2020-01-28     2 recovered
    ## 269         Guangxi Mainland China 23.82980 108.7881 2020-01-28     2 recovered
    ## 270           Hubei Mainland China 30.97560 112.2707 2020-01-28    35 recovered
    ## 271         Jiangxi Mainland China 27.61400 115.7221 2020-01-28     1 recovered
    ## 272        Shanghai Mainland China 31.20200 121.4491 2020-01-28     1 recovered
    ## 273        Zhejiang Mainland China 29.18320 120.0934 2020-01-28     2 recovered
    ## 278           Anhui Mainland China 31.82570 117.2264 2020-01-29    46 confirmed
    ## 279         Beijing Mainland China 40.18240 116.4142 2020-01-29    20 confirmed
    ## 280       Chongqing Mainland China 30.05720 107.8740 2020-01-29    15 confirmed
    ## 281          Fujian Mainland China 26.07890 117.9874 2020-01-29     4 confirmed
    ## 282           Gansu Mainland China 36.06110 103.8343 2020-01-29     5 confirmed
    ## 283       Guangdong Mainland China 23.34170 113.4244 2020-01-29    70 confirmed
    ## 284         Guangxi Mainland China 23.82980 108.7881 2020-01-29     7 confirmed
    ## 285          Hainan Mainland China 19.19590 109.7453 2020-01-29     3 confirmed
    ## 286           Hebei Mainland China 38.04280 114.5149 2020-01-29    15 confirmed
    ## 287    Heilongjiang Mainland China 47.86200 127.7615 2020-01-29     5 confirmed
    ## 288           Henan Mainland China 33.88202 113.6140 2020-01-29    38 confirmed
    ## 290           Hunan Mainland China 27.61040 111.7088 2020-01-29    78 confirmed
    ## 291  Inner Mongolia Mainland China 44.09350 113.9448 2020-01-29     1 confirmed
    ## 292         Jiangsu Mainland China 32.97110 119.4550 2020-01-29    29 confirmed
    ## 293           Jilin Mainland China 43.66610 126.1923 2020-01-29     1 confirmed
    ## 294        Liaoning Mainland China 41.29560 122.6085 2020-01-29     5 confirmed
    ## 295         Ningxia Mainland China 37.26920 106.1655 2020-01-29     1 confirmed
    ## 297         Shaanxi Mainland China 35.19170 108.8701 2020-01-29    10 confirmed
    ## 298        Shandong Mainland China 36.34270 118.1498 2020-01-29    35 confirmed
    ## 299        Shanghai Mainland China 31.20200 121.4491 2020-01-29    30 confirmed
    ## 300         Sichuan Mainland China 30.61710 102.7103 2020-01-29    18 confirmed
    ## 301         Tianjin Mainland China 39.30540 117.3230 2020-01-29     3 confirmed
    ## 302        Xinjiang Mainland China 41.11290  85.2401 2020-01-29     3 confirmed
    ## 303          Yunnan Mainland China 24.97400 101.4870 2020-01-29    11 confirmed
    ## 304        Zhejiang Mainland China 29.18320 120.0934 2020-01-29   123 confirmed
    ## 305           Henan Mainland China 33.88202 113.6140 2020-01-29     1     death
    ## 306         Sichuan Mainland China 30.61710 102.7103 2020-01-29     1     death
    ## 307           Anhui Mainland China 31.82570 117.2264 2020-01-29     2 recovered
    ## 308       Chongqing Mainland China 30.05720 107.8740 2020-01-29     1 recovered
    ## 309       Guangdong Mainland China 23.34170 113.4244 2020-01-29     1 recovered
    ## 310         Guizhou Mainland China 26.81540 106.8748 2020-01-29     1 recovered
    ## 311           Henan Mainland China 33.88202 113.6140 2020-01-29     1 recovered
    ## 312           Hubei Mainland China 30.97560 112.2707 2020-01-29     8 recovered
    ## 313        Liaoning Mainland China 41.29560 122.6085 2020-01-29     1 recovered
    ## 314        Shandong Mainland China 36.34270 118.1498 2020-01-29     1 recovered
    ## 315        Shanghai Mainland China 31.20200 121.4491 2020-01-29     1 recovered
    ## 316          Shanxi Mainland China 37.57770 112.2922 2020-01-29     1 recovered
    ## 317         Sichuan Mainland China 30.61710 102.7103 2020-01-29     1 recovered
    ## 323           Anhui Mainland China 31.82570 117.2264 2020-01-30    48 confirmed
    ## 324         Beijing Mainland China 40.18240 116.4142 2020-01-30     3 confirmed
    ## 325       Chongqing Mainland China 30.05720 107.8740 2020-01-30    35 confirmed
    ## 326          Fujian Mainland China 26.07890 117.9874 2020-01-30    17 confirmed
    ## 327           Gansu Mainland China 36.06110 103.8343 2020-01-30     2 confirmed
    ## 328       Guangdong Mainland China 23.34170 113.4244 2020-01-30    77 confirmed
    ## 329         Guangxi Mainland China 23.82980 108.7881 2020-01-30    20 confirmed
    ## 330         Guizhou Mainland China 26.81540 106.8748 2020-01-30     3 confirmed
    ## 331          Hainan Mainland China 19.19590 109.7453 2020-01-30     3 confirmed
    ## 332           Hebei Mainland China 38.04280 114.5149 2020-01-30    17 confirmed
    ## 333    Heilongjiang Mainland China 47.86200 127.7615 2020-01-30     6 confirmed
    ## 334           Henan Mainland China 33.88202 113.6140 2020-01-30    72 confirmed
    ## 335           Hubei Mainland China 30.97560 112.2707 2020-01-30  1349 confirmed
    ## 336           Hunan Mainland China 27.61040 111.7088 2020-01-30    56 confirmed
    ## 337  Inner Mongolia Mainland China 44.09350 113.9448 2020-01-30     3 confirmed
    ## 338         Jiangsu Mainland China 32.97110 119.4550 2020-01-30    30 confirmed
    ## 339         Jiangxi Mainland China 27.61400 115.7221 2020-01-30    53 confirmed
    ## 340           Jilin Mainland China 43.66610 126.1923 2020-01-30     5 confirmed
    ## 341        Liaoning Mainland China 41.29560 122.6085 2020-01-30     2 confirmed
    ## 342         Ningxia Mainland China 37.26920 106.1655 2020-01-30     5 confirmed
    ## 343         Qinghai Mainland China 35.74520  95.9956 2020-01-30     2 confirmed
    ## 345         Shaanxi Mainland China 35.19170 108.8701 2020-01-30     7 confirmed
    ## 346        Shandong Mainland China 36.34270 118.1498 2020-01-30    28 confirmed
    ## 347        Shanghai Mainland China 31.20200 121.4491 2020-01-30    16 confirmed
    ## 348          Shanxi Mainland China 37.57770 112.2922 2020-01-30     8 confirmed
    ## 349         Sichuan Mainland China 30.61710 102.7103 2020-01-30    34 confirmed
    ## 351         Tianjin Mainland China 39.30540 117.3230 2020-01-30     4 confirmed
    ## 352           Tibet Mainland China 31.69270  88.0924 2020-01-30     1 confirmed
    ## 354        Xinjiang Mainland China 41.11290  85.2401 2020-01-30     1 confirmed
    ## 355          Yunnan Mainland China 24.97400 101.4870 2020-01-30    15 confirmed
    ## 356        Zhejiang Mainland China 29.18320 120.0934 2020-01-30   132 confirmed
    ## 357    Heilongjiang Mainland China 47.86200 127.7615 2020-01-30     1     death
    ## 358           Hubei Mainland China 30.97560 112.2707 2020-01-30    37     death
    ## 359       Guangdong Mainland China 23.34170 113.4244 2020-01-30     5 recovered
    ## 360          Hainan Mainland China 19.19590 109.7453 2020-01-30     1 recovered
    ## 361           Henan Mainland China 33.88202 113.6140 2020-01-30     1 recovered
    ## 362           Hubei Mainland China 30.97560 112.2707 2020-01-30     2 recovered
    ## 363           Hunan Mainland China 27.61040 111.7088 2020-01-30     2 recovered
    ## 364         Jiangxi Mainland China 27.61400 115.7221 2020-01-30     2 recovered
    ## 365           Jilin Mainland China 43.66610 126.1923 2020-01-30     1 recovered
    ## 367        Zhejiang Mainland China 29.18320 120.0934 2020-01-30     1 recovered
    ## 377           Anhui Mainland China 31.82570 117.2264 2020-01-31    37 confirmed
    ## 378         Beijing Mainland China 40.18240 116.4142 2020-01-31    25 confirmed
    ## 379       Chongqing Mainland China 30.05720 107.8740 2020-01-31    29 confirmed
    ## 381          Fujian Mainland China 26.07890 117.9874 2020-01-31    19 confirmed
    ## 382           Gansu Mainland China 36.06110 103.8343 2020-01-31     3 confirmed
    ## 383       Guangdong Mainland China 23.34170 113.4244 2020-01-31    82 confirmed
    ## 384         Guangxi Mainland China 23.82980 108.7881 2020-01-31     9 confirmed
    ## 385         Guizhou Mainland China 26.81540 106.8748 2020-01-31    17 confirmed
    ## 386          Hainan Mainland China 19.19590 109.7453 2020-01-31     6 confirmed
    ## 387           Hebei Mainland China 38.04280 114.5149 2020-01-31    17 confirmed
    ## 388    Heilongjiang Mainland China 47.86200 127.7615 2020-01-31    15 confirmed
    ## 389           Henan Mainland China 33.88202 113.6140 2020-01-31    74 confirmed
    ## 391           Hubei Mainland China 30.97560 112.2707 2020-01-31   903 confirmed
    ## 392           Hunan Mainland China 27.61040 111.7088 2020-01-31    55 confirmed
    ## 393  Inner Mongolia Mainland China 44.09350 113.9448 2020-01-31     1 confirmed
    ## 394         Jiangsu Mainland China 32.97110 119.4550 2020-01-31    39 confirmed
    ## 395         Jiangxi Mainland China 27.61400 115.7221 2020-01-31    78 confirmed
    ## 396        Liaoning Mainland China 41.29560 122.6085 2020-01-31     7 confirmed
    ## 398         Ningxia Mainland China 37.26920 106.1655 2020-01-31     4 confirmed
    ## 401         Shaanxi Mainland China 35.19170 108.8701 2020-01-31    24 confirmed
    ## 402        Shandong Mainland China 36.34270 118.1498 2020-01-31    26 confirmed
    ## 403        Shanghai Mainland China 31.20200 121.4491 2020-01-31    23 confirmed
    ## 404          Shanxi Mainland China 37.57770 112.2922 2020-01-31     4 confirmed
    ## 405         Sichuan Mainland China 30.61710 102.7103 2020-01-31    35 confirmed
    ## 407         Tianjin Mainland China 39.30540 117.3230 2020-01-31     1 confirmed
    ## 410        Xinjiang Mainland China 41.11290  85.2401 2020-01-31     3 confirmed
    ## 411          Yunnan Mainland China 24.97400 101.4870 2020-01-31    13 confirmed
    ## 412        Zhejiang Mainland China 29.18320 120.0934 2020-01-31   110 confirmed
    ## 413           Hubei Mainland China 30.97560 112.2707 2020-01-31    42     death
    ## 414           Anhui Mainland China 31.82570 117.2264 2020-01-31     1 recovered
    ## 415         Beijing Mainland China 40.18240 116.4142 2020-01-31     1 recovered
    ## 416       Guangdong Mainland China 23.34170 113.4244 2020-01-31     1 recovered
    ## 417         Guizhou Mainland China 26.81540 106.8748 2020-01-31     1 recovered
    ## 418           Henan Mainland China 33.88202 113.6140 2020-01-31     1 recovered
    ## 419           Hubei Mainland China 30.97560 112.2707 2020-01-31    51 recovered
    ## 420  Inner Mongolia Mainland China 44.09350 113.9448 2020-01-31     1 recovered
    ## 421         Jiangsu Mainland China 32.97110 119.4550 2020-01-31     4 recovered
    ## 422         Jiangxi Mainland China 27.61400 115.7221 2020-01-31     2 recovered
    ## 423        Shandong Mainland China 36.34270 118.1498 2020-01-31     1 recovered
    ## 424        Shanghai Mainland China 31.20200 121.4491 2020-01-31     4 recovered
    ## 425          Yunnan Mainland China 24.97400 101.4870 2020-01-31     1 recovered
    ## 426        Zhejiang Mainland China 29.18320 120.0934 2020-01-31    10 recovered
    ## 434           Anhui Mainland China 31.82570 117.2264 2020-02-01    60 confirmed
    ## 435         Beijing Mainland China 40.18240 116.4142 2020-02-01    29 confirmed
    ## 437       Chongqing Mainland China 30.05720 107.8740 2020-02-01    36 confirmed
    ## 438          Fujian Mainland China 26.07890 117.9874 2020-02-01    24 confirmed
    ## 439           Gansu Mainland China 36.06110 103.8343 2020-02-01    11 confirmed
    ## 440       Guangdong Mainland China 23.34170 113.4244 2020-02-01    99 confirmed
    ## 441         Guangxi Mainland China 23.82980 108.7881 2020-02-01    13 confirmed
    ## 442          Hainan Mainland China 19.19590 109.7453 2020-02-01    10 confirmed
    ## 443           Hebei Mainland China 38.04280 114.5149 2020-02-01    14 confirmed
    ## 444    Heilongjiang Mainland China 47.86200 127.7615 2020-02-01    21 confirmed
    ## 445           Henan Mainland China 33.88202 113.6140 2020-02-01    70 confirmed
    ## 447           Hubei Mainland China 30.97560 112.2707 2020-02-01  1347 confirmed
    ## 448           Hunan Mainland China 27.61040 111.7088 2020-02-01    57 confirmed
    ## 449  Inner Mongolia Mainland China 44.09350 113.9448 2020-02-01     3 confirmed
    ## 450         Jiangsu Mainland China 32.97110 119.4550 2020-02-01    34 confirmed
    ## 451         Jiangxi Mainland China 27.61400 115.7221 2020-02-01    46 confirmed
    ## 452           Jilin Mainland China 43.66610 126.1923 2020-02-01     3 confirmed
    ## 453        Liaoning Mainland China 41.29560 122.6085 2020-02-01    16 confirmed
    ## 454         Ningxia Mainland China 37.26920 106.1655 2020-02-01     5 confirmed
    ## 455         Qinghai Mainland China 35.74520  95.9956 2020-02-01     1 confirmed
    ## 457         Shaanxi Mainland China 35.19170 108.8701 2020-02-01    14 confirmed
    ## 458        Shandong Mainland China 36.34270 118.1498 2020-02-01    22 confirmed
    ## 459        Shanghai Mainland China 31.20200 121.4491 2020-02-01    34 confirmed
    ## 460          Shanxi Mainland China 37.57770 112.2922 2020-02-01     8 confirmed
    ## 461         Sichuan Mainland China 30.61710 102.7103 2020-02-01    30 confirmed
    ## 463         Tianjin Mainland China 39.30540 117.3230 2020-02-01     9 confirmed
    ## 465        Xinjiang Mainland China 41.11290  85.2401 2020-02-01     1 confirmed
    ## 466          Yunnan Mainland China 24.97400 101.4870 2020-02-01    10 confirmed
    ## 467        Zhejiang Mainland China 29.18320 120.0934 2020-02-01    61 confirmed
    ## 468       Chongqing Mainland China 30.05720 107.8740 2020-02-01     1     death
    ## 469           Hubei Mainland China 30.97560 112.2707 2020-02-01    45     death
    ## 471           Anhui Mainland China 31.82570 117.2264 2020-02-01     2 recovered
    ## 472         Beijing Mainland China 40.18240 116.4142 2020-02-01     4 recovered
    ## 473       Chongqing Mainland China 30.05720 107.8740 2020-02-01     2 recovered
    ## 474       Guangdong Mainland China 23.34170 113.4244 2020-02-01     3 recovered
    ## 475    Heilongjiang Mainland China 47.86200 127.7615 2020-02-01     2 recovered
    ## 476           Hubei Mainland China 30.97560 112.2707 2020-02-01    27 recovered
    ## 477           Hunan Mainland China 27.61040 111.7088 2020-02-01     6 recovered
    ## 478         Jiangsu Mainland China 32.97110 119.4550 2020-02-01     1 recovered
    ## 479         Jiangxi Mainland China 27.61400 115.7221 2020-02-01     2 recovered
    ## 480        Shandong Mainland China 36.34270 118.1498 2020-02-01     1 recovered
    ## 481        Shanghai Mainland China 31.20200 121.4491 2020-02-01     1 recovered
    ## 482         Sichuan Mainland China 30.61710 102.7103 2020-02-01     2 recovered
    ## 483          Yunnan Mainland China 24.97400 101.4870 2020-02-01     1 recovered
    ## 484        Zhejiang Mainland China 29.18320 120.0934 2020-02-01     7 recovered
    ## 491           Anhui Mainland China 31.82570 117.2264 2020-02-02    43 confirmed
    ## 492         Beijing Mainland China 40.18240 116.4142 2020-02-02    23 confirmed
    ## 493       Chongqing Mainland China 30.05720 107.8740 2020-02-02    53 confirmed
    ## 494          Fujian Mainland China 26.07890 117.9874 2020-02-02    15 confirmed
    ## 495           Gansu Mainland China 36.06110 103.8343 2020-02-02    11 confirmed
    ## 496       Guangdong Mainland China 23.34170 113.4244 2020-02-02    97 confirmed
    ## 497         Guangxi Mainland China 23.82980 108.7881 2020-02-02    11 confirmed
    ## 498         Guizhou Mainland China 26.81540 106.8748 2020-02-02     9 confirmed
    ## 499          Hainan Mainland China 19.19590 109.7453 2020-02-02     2 confirmed
    ## 500           Hebei Mainland China 38.04280 114.5149 2020-02-02     8 confirmed
    ## 501    Heilongjiang Mainland China 47.86200 127.7615 2020-02-02    15 confirmed
    ## 502           Henan Mainland China 33.88202 113.6140 2020-02-02    71 confirmed
    ## 504           Hubei Mainland China 30.97560 112.2707 2020-02-02  4024 confirmed
    ## 505           Hunan Mainland China 27.61040 111.7088 2020-02-02    74 confirmed
    ## 506  Inner Mongolia Mainland China 44.09350 113.9448 2020-02-02     4 confirmed
    ## 507         Jiangsu Mainland China 32.97110 119.4550 2020-02-02    34 confirmed
    ## 508         Jiangxi Mainland China 27.61400 115.7221 2020-02-02    47 confirmed
    ## 509           Jilin Mainland China 43.66610 126.1923 2020-02-02     6 confirmed
    ## 510        Liaoning Mainland China 41.29560 122.6085 2020-02-02     6 confirmed
    ## 512         Ningxia Mainland China 37.26920 106.1655 2020-02-02     2 confirmed
    ## 513         Qinghai Mainland China 35.74520  95.9956 2020-02-02     2 confirmed
    ## 515         Shaanxi Mainland China 35.19170 108.8701 2020-02-02    15 confirmed
    ## 516        Shandong Mainland China 36.34270 118.1498 2020-02-02    24 confirmed
    ## 517        Shanghai Mainland China 31.20200 121.4491 2020-02-02    13 confirmed
    ## 518          Shanxi Mainland China 37.57770 112.2922 2020-02-02    19 confirmed
    ## 519         Sichuan Mainland China 30.61710 102.7103 2020-02-02    24 confirmed
    ## 521         Tianjin Mainland China 39.30540 117.3230 2020-02-02     7 confirmed
    ## 522        Xinjiang Mainland China 41.11290  85.2401 2020-02-02     3 confirmed
    ## 523          Yunnan Mainland China 24.97400 101.4870 2020-02-02    12 confirmed
    ## 524        Zhejiang Mainland China 29.18320 120.0934 2020-02-02    62 confirmed
    ## 526       Chongqing Mainland China 30.05720 107.8740 2020-02-02     1     death
    ## 527           Hubei Mainland China 30.97560 112.2707 2020-02-02   101     death
    ## 528           Anhui Mainland China 31.82570 117.2264 2020-02-02     2 recovered
    ## 529       Chongqing Mainland China 30.05720 107.8740 2020-02-02     4 recovered
    ## 530           Gansu Mainland China 36.06110 103.8343 2020-02-02     3 recovered
    ## 531       Guangdong Mainland China 23.34170 113.4244 2020-02-02     1 recovered
    ## 532          Hainan Mainland China 19.19590 109.7453 2020-02-02     3 recovered
    ## 533           Hebei Mainland China 38.04280 114.5149 2020-02-02     3 recovered
    ## 534           Henan Mainland China 33.88202 113.6140 2020-02-02     7 recovered
    ## 535           Hubei Mainland China 30.97560 112.2707 2020-02-02   127 recovered
    ## 536           Hunan Mainland China 27.61040 111.7088 2020-02-02     8 recovered
    ## 537         Jiangsu Mainland China 32.97110 119.4550 2020-02-02     1 recovered
    ## 538         Jiangxi Mainland China 27.61400 115.7221 2020-02-02     3 recovered
    ## 539        Shandong Mainland China 36.34270 118.1498 2020-02-02     3 recovered
    ## 540          Shanxi Mainland China 37.57770 112.2922 2020-02-02     2 recovered
    ## 541         Sichuan Mainland China 30.61710 102.7103 2020-02-02     8 recovered
    ## 542         Tianjin Mainland China 39.30540 117.3230 2020-02-02     1 recovered
    ## 543          Yunnan Mainland China 24.97400 101.4870 2020-02-02     1 recovered
    ## 544        Zhejiang Mainland China 29.18320 120.0934 2020-02-02    11 recovered
    ## 548           Anhui Mainland China 31.82570 117.2264 2020-02-03    68 confirmed
    ## 549         Beijing Mainland China 40.18240 116.4142 2020-02-03    21 confirmed
    ## 550       Chongqing Mainland China 30.05720 107.8740 2020-02-03    37 confirmed
    ## 551          Fujian Mainland China 26.07890 117.9874 2020-02-03    20 confirmed
    ## 552           Gansu Mainland China 36.06110 103.8343 2020-02-03     4 confirmed
    ## 553       Guangdong Mainland China 23.34170 113.4244 2020-02-03    93 confirmed
    ## 554         Guangxi Mainland China 23.82980 108.7881 2020-02-03    16 confirmed
    ## 555         Guizhou Mainland China 26.81540 106.8748 2020-02-03     8 confirmed
    ## 556          Hainan Mainland China 19.19590 109.7453 2020-02-03     8 confirmed
    ## 557           Hebei Mainland China 38.04280 114.5149 2020-02-03     9 confirmed
    ## 558    Heilongjiang Mainland China 47.86200 127.7615 2020-02-03    26 confirmed
    ## 559           Henan Mainland China 33.88202 113.6140 2020-02-03    73 confirmed
    ## 560           Hubei Mainland China 30.97560 112.2707 2020-02-03  2345 confirmed
    ## 561           Hunan Mainland China 27.61040 111.7088 2020-02-03    58 confirmed
    ## 562  Inner Mongolia Mainland China 44.09350 113.9448 2020-02-03     7 confirmed
    ## 563         Jiangsu Mainland China 32.97110 119.4550 2020-02-03    35 confirmed
    ## 564         Jiangxi Mainland China 27.61400 115.7221 2020-02-03    58 confirmed
    ## 565           Jilin Mainland China 43.66610 126.1923 2020-02-03     8 confirmed
    ## 566        Liaoning Mainland China 41.29560 122.6085 2020-02-03     4 confirmed
    ## 567         Ningxia Mainland China 37.26920 106.1655 2020-02-03     3 confirmed
    ## 568         Qinghai Mainland China 35.74520  95.9956 2020-02-03     2 confirmed
    ## 571         Shaanxi Mainland China 35.19170 108.8701 2020-02-03    12 confirmed
    ## 572        Shandong Mainland China 36.34270 118.1498 2020-02-03    29 confirmed
    ## 573        Shanghai Mainland China 31.20200 121.4491 2020-02-03    21 confirmed
    ## 574          Shanxi Mainland China 37.57770 112.2922 2020-02-03     8 confirmed
    ## 575         Sichuan Mainland China 30.61710 102.7103 2020-02-03    23 confirmed
    ## 576         Tianjin Mainland China 39.30540 117.3230 2020-02-03    12 confirmed
    ## 577        Xinjiang Mainland China 41.11290  85.2401 2020-02-03     3 confirmed
    ## 578          Yunnan Mainland China 24.97400 101.4870 2020-02-03    12 confirmed
    ## 579        Zhejiang Mainland China 29.18320 120.0934 2020-02-03    63 confirmed
    ## 580           Hubei Mainland China 30.97560 112.2707 2020-02-03    64     death
    ## 581           Anhui Mainland China 31.82570 117.2264 2020-02-03     7 recovered
    ## 582         Beijing Mainland China 40.18240 116.4142 2020-02-03     3 recovered
    ## 583       Chongqing Mainland China 30.05720 107.8740 2020-02-03     2 recovered
    ## 584          Fujian Mainland China 26.07890 117.9874 2020-02-03     1 recovered
    ## 585       Guangdong Mainland China 23.34170 113.4244 2020-02-03     6 recovered
    ## 586         Guangxi Mainland China 23.82980 108.7881 2020-02-03     5 recovered
    ## 587           Henan Mainland China 33.88202 113.6140 2020-02-03     6 recovered
    ## 588           Hubei Mainland China 30.97560 112.2707 2020-02-03    91 recovered
    ## 589           Hunan Mainland China 27.61040 111.7088 2020-02-03     6 recovered
    ## 590         Jiangsu Mainland China 32.97110 119.4550 2020-02-03     1 recovered
    ## 591         Jiangxi Mainland China 27.61400 115.7221 2020-02-03     6 recovered
    ## 592         Ningxia Mainland China 37.26920 106.1655 2020-02-03     1 recovered
    ## 593        Shandong Mainland China 36.34270 118.1498 2020-02-03     1 recovered
    ## 594          Shanxi Mainland China 37.57770 112.2922 2020-02-03    -1 recovered
    ## 595         Sichuan Mainland China 30.61710 102.7103 2020-02-03     3 recovered
    ## 596          Yunnan Mainland China 24.97400 101.4870 2020-02-03     2 recovered
    ## 597        Zhejiang Mainland China 29.18320 120.0934 2020-02-03    11 recovered
    ## 604           Anhui Mainland China 31.82570 117.2264 2020-02-04    72 confirmed
    ## 605         Beijing Mainland China 40.18240 116.4142 2020-02-04    16 confirmed
    ## 606       Chongqing Mainland China 30.05720 107.8740 2020-02-04    29 confirmed
    ## 607          Fujian Mainland China 26.07890 117.9874 2020-02-04    15 confirmed
    ## 608           Gansu Mainland China 36.06110 103.8343 2020-02-04     2 confirmed
    ## 609       Guangdong Mainland China 23.34170 113.4244 2020-02-04    88 confirmed
    ## 610         Guangxi Mainland China 23.82980 108.7881 2020-02-04    12 confirmed
    ## 611         Guizhou Mainland China 26.81540 106.8748 2020-02-04    12 confirmed
    ## 612          Hainan Mainland China 19.19590 109.7453 2020-02-04     8 confirmed
    ## 613           Hebei Mainland China 38.04280 114.5149 2020-02-04    13 confirmed
    ## 614    Heilongjiang Mainland China 47.86200 127.7615 2020-02-04    34 confirmed
    ## 615           Henan Mainland China 33.88202 113.6140 2020-02-04   109 confirmed
    ## 617           Hubei Mainland China 30.97560 112.2707 2020-02-04  3156 confirmed
    ## 618           Hunan Mainland China 27.61040 111.7088 2020-02-04    72 confirmed
    ## 619  Inner Mongolia Mainland China 44.09350 113.9448 2020-02-04     1 confirmed
    ## 620         Jiangsu Mainland China 32.97110 119.4550 2020-02-04    37 confirmed
    ## 621         Jiangxi Mainland China 27.61400 115.7221 2020-02-04    85 confirmed
    ## 622           Jilin Mainland China 43.66610 126.1923 2020-02-04    11 confirmed
    ## 623        Liaoning Mainland China 41.29560 122.6085 2020-02-04     7 confirmed
    ## 625         Ningxia Mainland China 37.26920 106.1655 2020-02-04     3 confirmed
    ## 626         Qinghai Mainland China 35.74520  95.9956 2020-02-04     2 confirmed
    ## 628         Shaanxi Mainland China 35.19170 108.8701 2020-02-04    14 confirmed
    ## 629        Shandong Mainland China 36.34270 118.1498 2020-02-04    16 confirmed
    ## 630        Shanghai Mainland China 31.20200 121.4491 2020-02-04    16 confirmed
    ## 631          Shanxi Mainland China 37.57770 112.2922 2020-02-04     7 confirmed
    ## 632         Sichuan Mainland China 30.61710 102.7103 2020-02-04    28 confirmed
    ## 634         Tianjin Mainland China 39.30540 117.3230 2020-02-04     7 confirmed
    ## 635        Xinjiang Mainland China 41.11290  85.2401 2020-02-04     5 confirmed
    ## 636          Yunnan Mainland China 24.97400 101.4870 2020-02-04     5 confirmed
    ## 637        Zhejiang Mainland China 29.18320 120.0934 2020-02-04   105 confirmed
    ## 639           Hubei Mainland China 30.97560 112.2707 2020-02-04    65     death
    ## 640           Anhui Mainland China 31.82570 117.2264 2020-02-04     6 recovered
    ## 641         Beijing Mainland China 40.18240 116.4142 2020-02-04    11 recovered
    ## 642          Fujian Mainland China 26.07890 117.9874 2020-02-04     2 recovered
    ## 643           Gansu Mainland China 36.06110 103.8343 2020-02-04     1 recovered
    ## 644       Guangdong Mainland China 23.34170 113.4244 2020-02-04     9 recovered
    ## 645         Guangxi Mainland China 23.82980 108.7881 2020-02-04     3 recovered
    ## 646          Hainan Mainland China 19.19590 109.7453 2020-02-04     1 recovered
    ## 647           Hebei Mainland China 38.04280 114.5149 2020-02-04     1 recovered
    ## 648    Heilongjiang Mainland China 47.86200 127.7615 2020-02-04     2 recovered
    ## 649           Henan Mainland China 33.88202 113.6140 2020-02-04    11 recovered
    ## 650           Hubei Mainland China 30.97560 112.2707 2020-02-04   136 recovered
    ## 651           Hunan Mainland China 27.61040 111.7088 2020-02-04     9 recovered
    ## 652         Jiangsu Mainland China 32.97110 119.4550 2020-02-04     4 recovered
    ## 653         Jiangxi Mainland China 27.61400 115.7221 2020-02-04     2 recovered
    ## 654        Liaoning Mainland China 41.29560 122.6085 2020-02-04     1 recovered
    ## 655         Shaanxi Mainland China 35.19170 108.8701 2020-02-04     2 recovered
    ## 656        Shandong Mainland China 36.34270 118.1498 2020-02-04     4 recovered
    ## 657        Shanghai Mainland China 31.20200 121.4491 2020-02-04     2 recovered
    ## 658          Shanxi Mainland China 37.57770 112.2922 2020-02-04     2 recovered
    ## 659         Tianjin Mainland China 39.30540 117.3230 2020-02-04     1 recovered
    ## 660        Zhejiang Mainland China 29.18320 120.0934 2020-02-04    19 recovered
    ## 664           Anhui Mainland China 31.82570 117.2264 2020-02-05    50 confirmed
    ## 665         Beijing Mainland China 40.18240 116.4142 2020-02-05    25 confirmed
    ## 667       Chongqing Mainland China 30.05720 107.8740 2020-02-05    23 confirmed
    ## 668          Fujian Mainland China 26.07890 117.9874 2020-02-05    11 confirmed
    ## 669           Gansu Mainland China 36.06110 103.8343 2020-02-05     5 confirmed
    ## 670       Guangdong Mainland China 23.34170 113.4244 2020-02-05    82 confirmed
    ## 671         Guangxi Mainland China 23.82980 108.7881 2020-02-05    11 confirmed
    ## 672         Guizhou Mainland China 26.81540 106.8748 2020-02-05     6 confirmed
    ## 673          Hainan Mainland China 19.19590 109.7453 2020-02-05    19 confirmed
    ## 674           Hebei Mainland China 38.04280 114.5149 2020-02-05     9 confirmed
    ## 675    Heilongjiang Mainland China 47.86200 127.7615 2020-02-05    35 confirmed
    ## 676           Henan Mainland China 33.88202 113.6140 2020-02-05    89 confirmed
    ## 678           Hubei Mainland China 30.97560 112.2707 2020-02-05  2987 confirmed
    ## 679           Hunan Mainland China 27.61040 111.7088 2020-02-05    68 confirmed
    ## 680  Inner Mongolia Mainland China 44.09350 113.9448 2020-02-05     7 confirmed
    ## 681         Jiangsu Mainland China 32.97110 119.4550 2020-02-05    33 confirmed
    ## 682         Jiangxi Mainland China 27.61400 115.7221 2020-02-05    72 confirmed
    ## 683           Jilin Mainland China 43.66610 126.1923 2020-02-05    12 confirmed
    ## 684        Liaoning Mainland China 41.29560 122.6085 2020-02-05     8 confirmed
    ## 686         Qinghai Mainland China 35.74520  95.9956 2020-02-05     2 confirmed
    ## 687         Shaanxi Mainland China 35.19170 108.8701 2020-02-05    23 confirmed
    ## 688        Shandong Mainland China 36.34270 118.1498 2020-02-05    32 confirmed
    ## 689        Shanghai Mainland China 31.20200 121.4491 2020-02-05    24 confirmed
    ## 690         Sichuan Mainland China 30.61710 102.7103 2020-02-05    19 confirmed
    ## 691         Tianjin Mainland China 39.30540 117.3230 2020-02-05     2 confirmed
    ## 692        Xinjiang Mainland China 41.11290  85.2401 2020-02-05     3 confirmed
    ## 693          Yunnan Mainland China 24.97400 101.4870 2020-02-05     6 confirmed
    ## 694        Zhejiang Mainland China 29.18320 120.0934 2020-02-05    66 confirmed
    ## 695         Guizhou Mainland China 26.81540 106.8748 2020-02-05     1     death
    ## 696           Hubei Mainland China 30.97560 112.2707 2020-02-05    70     death
    ## 697         Tianjin Mainland China 39.30540 117.3230 2020-02-05     1     death
    ## 698           Anhui Mainland China 31.82570 117.2264 2020-02-05     3 recovered
    ## 699         Beijing Mainland China 40.18240 116.4142 2020-02-05     1 recovered
    ## 700       Chongqing Mainland China 30.05720 107.8740 2020-02-05     6 recovered
    ## 701          Fujian Mainland China 26.07890 117.9874 2020-02-05     8 recovered
    ## 702           Gansu Mainland China 36.06110 103.8343 2020-02-05     2 recovered
    ## 703       Guangdong Mainland China 23.34170 113.4244 2020-02-05    19 recovered
    ## 704         Guangxi Mainland China 23.82980 108.7881 2020-02-05     3 recovered
    ## 705         Guizhou Mainland China 26.81540 106.8748 2020-02-05     7 recovered
    ## 706           Hebei Mainland China 38.04280 114.5149 2020-02-05     2 recovered
    ## 707    Heilongjiang Mainland China 47.86200 127.7615 2020-02-05     3 recovered
    ## 708           Henan Mainland China 33.88202 113.6140 2020-02-05    20 recovered
    ## 709           Hubei Mainland China 30.97560 112.2707 2020-02-05   111 recovered
    ## 710           Hunan Mainland China 27.61040 111.7088 2020-02-05    23 recovered
    ## 711  Inner Mongolia Mainland China 44.09350 113.9448 2020-02-05     2 recovered
    ## 712         Jiangsu Mainland China 32.97110 119.4550 2020-02-05    11 recovered
    ## 713         Jiangxi Mainland China 27.61400 115.7221 2020-02-05     7 recovered
    ## 714           Jilin Mainland China 43.66610 126.1923 2020-02-05     1 recovered
    ## 715        Liaoning Mainland China 41.29560 122.6085 2020-02-05     2 recovered
    ## 716         Qinghai Mainland China 35.74520  95.9956 2020-02-05     3 recovered
    ## 717         Shaanxi Mainland China 35.19170 108.8701 2020-02-05     4 recovered
    ## 718        Shandong Mainland China 36.34270 118.1498 2020-02-05     4 recovered
    ## 719        Shanghai Mainland China 31.20200 121.4491 2020-02-05     3 recovered
    ## 720          Shanxi Mainland China 37.57770 112.2922 2020-02-05     1 recovered
    ## 721         Sichuan Mainland China 30.61710 102.7103 2020-02-05    10 recovered
    ## 722        Zhejiang Mainland China 29.18320 120.0934 2020-02-05    16 recovered
    ## 726           Anhui Mainland China 31.82570 117.2264 2020-02-06    61 confirmed
    ## 727         Beijing Mainland China 40.18240 116.4142 2020-02-06    21 confirmed
    ## 728       Chongqing Mainland China 30.05720 107.8740 2020-02-06    22 confirmed
    ## 729          Fujian Mainland China 26.07890 117.9874 2020-02-06    10 confirmed
    ## 730       Guangdong Mainland China 23.34170 113.4244 2020-02-06    75 confirmed
    ## 731         Guangxi Mainland China 23.82980 108.7881 2020-02-06    18 confirmed
    ## 732         Guizhou Mainland China 26.81540 106.8748 2020-02-06     7 confirmed
    ## 733          Hainan Mainland China 19.19590 109.7453 2020-02-06     7 confirmed
    ## 734           Hebei Mainland China 38.04280 114.5149 2020-02-06    22 confirmed
    ## 735    Heilongjiang Mainland China 47.86200 127.7615 2020-02-06    37 confirmed
    ## 736           Henan Mainland China 33.88202 113.6140 2020-02-06    87 confirmed
    ## 738           Hubei Mainland China 30.97560 112.2707 2020-02-06  2447 confirmed
    ## 739           Hunan Mainland China 27.61040 111.7088 2020-02-06    50 confirmed
    ## 740  Inner Mongolia Mainland China 44.09350 113.9448 2020-02-06     4 confirmed
    ## 741         Jiangsu Mainland China 32.97110 119.4550 2020-02-06    32 confirmed
    ## 742         Jiangxi Mainland China 27.61400 115.7221 2020-02-06    52 confirmed
    ## 743           Jilin Mainland China 43.66610 126.1923 2020-02-06     5 confirmed
    ## 744        Liaoning Mainland China 41.29560 122.6085 2020-02-06     5 confirmed
    ## 745         Ningxia Mainland China 37.26920 106.1655 2020-02-06     6 confirmed
    ## 746         Qinghai Mainland China 35.74520  95.9956 2020-02-06     1 confirmed
    ## 748         Shaanxi Mainland China 35.19170 108.8701 2020-02-06     8 confirmed
    ## 749        Shandong Mainland China 36.34270 118.1498 2020-02-06    40 confirmed
    ## 750        Shanghai Mainland China 31.20200 121.4491 2020-02-06    14 confirmed
    ## 751          Shanxi Mainland China 37.57770 112.2922 2020-02-06    15 confirmed
    ## 752         Sichuan Mainland China 30.61710 102.7103 2020-02-06    20 confirmed
    ## 754         Tianjin Mainland China 39.30540 117.3230 2020-02-06    10 confirmed
    ## 755        Xinjiang Mainland China 41.11290  85.2401 2020-02-06     4 confirmed
    ## 756          Yunnan Mainland China 24.97400 101.4870 2020-02-06     5 confirmed
    ## 757        Zhejiang Mainland China 29.18320 120.0934 2020-02-06    59 confirmed
    ## 758    Heilongjiang Mainland China 47.86200 127.7615 2020-02-06     1     death
    ## 759           Hubei Mainland China 30.97560 112.2707 2020-02-06    69     death
    ## 760           Anhui Mainland China 31.82570 117.2264 2020-02-06    11 recovered
    ## 761         Beijing Mainland China 40.18240 116.4142 2020-02-06     7 recovered
    ## 762       Chongqing Mainland China 30.05720 107.8740 2020-02-06     9 recovered
    ## 763          Fujian Mainland China 26.07890 117.9874 2020-02-06     3 recovered
    ## 764       Guangdong Mainland China 23.34170 113.4244 2020-02-06    20 recovered
    ## 765         Guangxi Mainland China 23.82980 108.7881 2020-02-06     1 recovered
    ## 766         Guizhou Mainland China 26.81540 106.8748 2020-02-06    -3 recovered
    ## 767          Hainan Mainland China 19.19590 109.7453 2020-02-06     3 recovered
    ## 768           Hebei Mainland China 38.04280 114.5149 2020-02-06     7 recovered
    ## 769    Heilongjiang Mainland China 47.86200 127.7615 2020-02-06     1 recovered
    ## 770           Henan Mainland China 33.88202 113.6140 2020-02-06     9 recovered
    ## 771           Hubei Mainland China 30.97560 112.2707 2020-02-06   184 recovered
    ## 772           Hunan Mainland China 27.61040 111.7088 2020-02-06    27 recovered
    ## 773  Inner Mongolia Mainland China 44.09350 113.9448 2020-02-06     1 recovered
    ## 774         Jiangsu Mainland China 32.97110 119.4550 2020-02-06    11 recovered
    ## 775         Jiangxi Mainland China 27.61400 115.7221 2020-02-06    10 recovered
    ## 776           Jilin Mainland China 43.66610 126.1923 2020-02-06     2 recovered
    ## 777        Liaoning Mainland China 41.29560 122.6085 2020-02-06     1 recovered
    ## 779         Shaanxi Mainland China 35.19170 108.8701 2020-02-06     3 recovered
    ## 780        Shandong Mainland China 36.34270 118.1498 2020-02-06    12 recovered
    ## 781        Shanghai Mainland China 31.20200 121.4491 2020-02-06    10 recovered
    ## 782          Shanxi Mainland China 37.57770 112.2922 2020-02-06     7 recovered
    ## 783         Sichuan Mainland China 30.61710 102.7103 2020-02-06     7 recovered
    ## 785          Yunnan Mainland China 24.97400 101.4870 2020-02-06     2 recovered
    ## 786        Zhejiang Mainland China 29.18320 120.0934 2020-02-06    16 recovered
    ## 794           Anhui Mainland China 31.82570 117.2264 2020-02-07    74 confirmed
    ## 795         Beijing Mainland China 40.18240 116.4142 2020-02-07    23 confirmed
    ## 797       Chongqing Mainland China 30.05720 107.8740 2020-02-07    15 confirmed
    ## 799          Fujian Mainland China 26.07890 117.9874 2020-02-07     9 confirmed
    ## 800           Gansu Mainland China 36.06110 103.8343 2020-02-07     5 confirmed
    ## 801       Guangdong Mainland China 23.34170 113.4244 2020-02-07    64 confirmed
    ## 802         Guangxi Mainland China 23.82980 108.7881 2020-02-07     4 confirmed
    ## 803         Guizhou Mainland China 26.81540 106.8748 2020-02-07    10 confirmed
    ## 804          Hainan Mainland China 19.19590 109.7453 2020-02-07    11 confirmed
    ## 805           Hebei Mainland China 38.04280 114.5149 2020-02-07    15 confirmed
    ## 806    Heilongjiang Mainland China 47.86200 127.7615 2020-02-07    50 confirmed
    ## 807           Henan Mainland China 33.88202 113.6140 2020-02-07    63 confirmed
    ## 809           Hubei Mainland China 30.97560 112.2707 2020-02-07  2841 confirmed
    ## 810           Hunan Mainland China 27.61040 111.7088 2020-02-07    61 confirmed
    ## 811  Inner Mongolia Mainland China 44.09350 113.9448 2020-02-07     4 confirmed
    ## 812         Jiangsu Mainland China 32.97110 119.4550 2020-02-07    35 confirmed
    ## 813         Jiangxi Mainland China 27.61400 115.7221 2020-02-07    61 confirmed
    ## 814           Jilin Mainland China 43.66610 126.1923 2020-02-07     6 confirmed
    ## 815        Liaoning Mainland China 41.29560 122.6085 2020-02-07     5 confirmed
    ## 816         Ningxia Mainland China 37.26920 106.1655 2020-02-07     3 confirmed
    ## 818         Shaanxi Mainland China 35.19170 108.8701 2020-02-07    11 confirmed
    ## 819        Shandong Mainland China 36.34270 118.1498 2020-02-07    39 confirmed
    ## 820        Shanghai Mainland China 31.20200 121.4491 2020-02-07    20 confirmed
    ## 821          Shanxi Mainland China 37.57770 112.2922 2020-02-07     8 confirmed
    ## 822         Sichuan Mainland China 30.61710 102.7103 2020-02-07    23 confirmed
    ## 823         Tianjin Mainland China 39.30540 117.3230 2020-02-07     2 confirmed
    ## 824        Xinjiang Mainland China 41.11290  85.2401 2020-02-07     3 confirmed
    ## 825          Yunnan Mainland China 24.97400 101.4870 2020-02-07     5 confirmed
    ## 826        Zhejiang Mainland China 29.18320 120.0934 2020-02-07    52 confirmed
    ## 827       Guangdong Mainland China 23.34170 113.4244 2020-02-07     1     death
    ## 828          Hainan Mainland China 19.19590 109.7453 2020-02-07     1     death
    ## 829           Henan Mainland China 33.88202 113.6140 2020-02-07     1     death
    ## 830           Hubei Mainland China 30.97560 112.2707 2020-02-07    81     death
    ## 831           Jilin Mainland China 43.66610 126.1923 2020-02-07     1     death
    ## 834           Anhui Mainland China 31.82570 117.2264 2020-02-07    13 recovered
    ## 835         Beijing Mainland China 40.18240 116.4142 2020-02-07     2 recovered
    ## 836       Chongqing Mainland China 30.05720 107.8740 2020-02-07     7 recovered
    ## 837          Fujian Mainland China 26.07890 117.9874 2020-02-07     6 recovered
    ## 838           Gansu Mainland China 36.06110 103.8343 2020-02-07     3 recovered
    ## 839       Guangdong Mainland China 23.34170 113.4244 2020-02-07    19 recovered
    ## 840         Guangxi Mainland China 23.82980 108.7881 2020-02-07     3 recovered
    ## 841          Hainan Mainland China 19.19590 109.7453 2020-02-07     2 recovered
    ## 842           Hebei Mainland China 38.04280 114.5149 2020-02-07     9 recovered
    ## 843    Heilongjiang Mainland China 47.86200 127.7615 2020-02-07     4 recovered
    ## 844           Henan Mainland China 33.88202 113.6140 2020-02-07    30 recovered
    ## 845           Hubei Mainland China 30.97560 112.2707 2020-02-07   298 recovered
    ## 846           Hunan Mainland China 27.61040 111.7088 2020-02-07    31 recovered
    ## 847  Inner Mongolia Mainland China 44.09350 113.9448 2020-02-07     1 recovered
    ## 848         Jiangsu Mainland China 32.97110 119.4550 2020-02-07     9 recovered
    ## 849         Jiangxi Mainland China 27.61400 115.7221 2020-02-07     8 recovered
    ## 850        Liaoning Mainland China 41.29560 122.6085 2020-02-07     2 recovered
    ## 851         Ningxia Mainland China 37.26920 106.1655 2020-02-07     4 recovered
    ## 852         Shaanxi Mainland China 35.19170 108.8701 2020-02-07     8 recovered
    ## 853        Shandong Mainland China 36.34270 118.1498 2020-02-07    10 recovered
    ## 854        Shanghai Mainland China 31.20200 121.4491 2020-02-07     5 recovered
    ## 855          Shanxi Mainland China 37.57770 112.2922 2020-02-07     3 recovered
    ## 856         Sichuan Mainland China 30.61710 102.7103 2020-02-07    11 recovered
    ## 857          Yunnan Mainland China 24.97400 101.4870 2020-02-07     5 recovered
    ## 858        Zhejiang Mainland China 29.18320 120.0934 2020-02-07    29 recovered
    ## 865           Anhui Mainland China 31.82570 117.2264 2020-02-08    68 confirmed
    ## 866         Beijing Mainland China 40.18240 116.4142 2020-02-08    18 confirmed
    ## 867       Chongqing Mainland China 30.05720 107.8740 2020-02-08     2 confirmed
    ## 868          Fujian Mainland China 26.07890 117.9874 2020-02-08    15 confirmed
    ## 869           Gansu Mainland China 36.06110 103.8343 2020-02-08    12 confirmed
    ## 870       Guangdong Mainland China 23.34170 113.4244 2020-02-08    61 confirmed
    ## 871         Guangxi Mainland China 23.82980 108.7881 2020-02-08    11 confirmed
    ## 872         Guizhou Mainland China 26.81540 106.8748 2020-02-08     8 confirmed
    ## 873          Hainan Mainland China 19.19590 109.7453 2020-02-08     7 confirmed
    ## 874           Hebei Mainland China 38.04280 114.5149 2020-02-08    23 confirmed
    ## 875    Heilongjiang Mainland China 47.86200 127.7615 2020-02-08    18 confirmed
    ## 876           Henan Mainland China 33.88202 113.6140 2020-02-08    67 confirmed
    ## 878           Hubei Mainland China 30.97560 112.2707 2020-02-08  2147 confirmed
    ## 879           Hunan Mainland China 27.61040 111.7088 2020-02-08    31 confirmed
    ## 880  Inner Mongolia Mainland China 44.09350 113.9448 2020-02-08     2 confirmed
    ## 881         Jiangsu Mainland China 32.97110 119.4550 2020-02-08    31 confirmed
    ## 882         Jiangxi Mainland China 27.61400 115.7221 2020-02-08    37 confirmed
    ## 883           Jilin Mainland China 43.66610 126.1923 2020-02-08     4 confirmed
    ## 884        Liaoning Mainland China 41.29560 122.6085 2020-02-08     6 confirmed
    ## 885         Ningxia Mainland China 37.26920 106.1655 2020-02-08     2 confirmed
    ## 886         Shaanxi Mainland China 35.19170 108.8701 2020-02-08    11 confirmed
    ## 887        Shandong Mainland China 36.34270 118.1498 2020-02-08    30 confirmed
    ## 888        Shanghai Mainland China 31.20200 121.4491 2020-02-08     9 confirmed
    ## 889          Shanxi Mainland China 37.57770 112.2922 2020-02-08    11 confirmed
    ## 890         Sichuan Mainland China 30.61710 102.7103 2020-02-08    20 confirmed
    ## 892         Tianjin Mainland China 39.30540 117.3230 2020-02-08     7 confirmed
    ## 893        Xinjiang Mainland China 41.11290  85.2401 2020-02-08     3 confirmed
    ## 894        Zhejiang Mainland China 29.18320 120.0934 2020-02-08    42 confirmed
    ## 895         Beijing Mainland China 40.18240 116.4142 2020-02-08     1     death
    ## 896           Gansu Mainland China 36.06110 103.8343 2020-02-08     1     death
    ## 897    Heilongjiang Mainland China 47.86200 127.7615 2020-02-08     2     death
    ## 898           Henan Mainland China 33.88202 113.6140 2020-02-08     1     death
    ## 899           Hubei Mainland China 30.97560 112.2707 2020-02-08    81     death
    ## 900           Hunan Mainland China 27.61040 111.7088 2020-02-08     1     death
    ## 904           Anhui Mainland China 31.82570 117.2264 2020-02-08    12 recovered
    ## 905         Beijing Mainland China 40.18240 116.4142 2020-02-08     1 recovered
    ## 906       Chongqing Mainland China 30.05720 107.8740 2020-02-08     8 recovered
    ## 907          Fujian Mainland China 26.07890 117.9874 2020-02-08     4 recovered
    ## 908           Gansu Mainland China 36.06110 103.8343 2020-02-08     3 recovered
    ## 909       Guangdong Mainland China 23.34170 113.4244 2020-02-08    24 recovered
    ## 910         Guizhou Mainland China 26.81540 106.8748 2020-02-08     1 recovered
    ## 911          Hainan Mainland China 19.19590 109.7453 2020-02-08     4 recovered
    ## 912           Hebei Mainland China 38.04280 114.5149 2020-02-08     8 recovered
    ## 913    Heilongjiang Mainland China 47.86200 127.7615 2020-02-08     1 recovered
    ## 914           Henan Mainland China 33.88202 113.6140 2020-02-08    30 recovered
    ## 915           Hubei Mainland China 30.97560 112.2707 2020-02-08   324 recovered
    ## 916           Hunan Mainland China 27.61040 111.7088 2020-02-08    44 recovered
    ## 917         Jiangsu Mainland China 32.97110 119.4550 2020-02-08     8 recovered
    ## 918         Jiangxi Mainland China 27.61400 115.7221 2020-02-08    10 recovered
    ## 919        Liaoning Mainland China 41.29560 122.6085 2020-02-08     1 recovered
    ## 920         Ningxia Mainland China 37.26920 106.1655 2020-02-08    10 recovered
    ## 921         Shaanxi Mainland China 35.19170 108.8701 2020-02-08     3 recovered
    ## 922        Shandong Mainland China 36.34270 118.1498 2020-02-08     7 recovered
    ## 923        Shanghai Mainland China 31.20200 121.4491 2020-02-08    11 recovered
    ## 924          Shanxi Mainland China 37.57770 112.2922 2020-02-08     6 recovered
    ## 925         Sichuan Mainland China 30.61710 102.7103 2020-02-08    18 recovered
    ## 926         Tianjin Mainland China 39.30540 117.3230 2020-02-08     2 recovered
    ## 927          Yunnan Mainland China 24.97400 101.4870 2020-02-08     5 recovered
    ## 928        Zhejiang Mainland China 29.18320 120.0934 2020-02-08    52 recovered
    ## 934           Anhui Mainland China 31.82570 117.2264 2020-02-09    46 confirmed
    ## 935         Beijing Mainland China 40.18240 116.4142 2020-02-09    11 confirmed
    ## 936       Chongqing Mainland China 30.05720 107.8740 2020-02-09    40 confirmed
    ## 938          Fujian Mainland China 26.07890 117.9874 2020-02-09    11 confirmed
    ## 939           Gansu Mainland China 36.06110 103.8343 2020-02-09     4 confirmed
    ## 940       Guangdong Mainland China 23.34170 113.4244 2020-02-09    36 confirmed
    ## 941         Guangxi Mainland China 23.82980 108.7881 2020-02-09    12 confirmed
    ## 942         Guizhou Mainland China 26.81540 106.8748 2020-02-09    10 confirmed
    ## 943          Hainan Mainland China 19.19590 109.7453 2020-02-09     7 confirmed
    ## 944           Hebei Mainland China 38.04280 114.5149 2020-02-09    11 confirmed
    ## 945    Heilongjiang Mainland China 47.86200 127.7615 2020-02-09    12 confirmed
    ## 946           Henan Mainland China 33.88202 113.6140 2020-02-09    52 confirmed
    ## 948           Hubei Mainland China 30.97560 112.2707 2020-02-09  2531 confirmed
    ## 949           Hunan Mainland China 27.61040 111.7088 2020-02-09    35 confirmed
    ## 950  Inner Mongolia Mainland China 44.09350 113.9448 2020-02-09     2 confirmed
    ## 951         Jiangsu Mainland China 32.97110 119.4550 2020-02-09    29 confirmed
    ## 952         Jiangxi Mainland China 27.61400 115.7221 2020-02-09    42 confirmed
    ## 953           Jilin Mainland China 43.66610 126.1923 2020-02-09     9 confirmed
    ## 954        Liaoning Mainland China 41.29560 122.6085 2020-02-09     2 confirmed
    ## 955         Shaanxi Mainland China 35.19170 108.8701 2020-02-09    13 confirmed
    ## 956        Shandong Mainland China 36.34270 118.1498 2020-02-09    28 confirmed
    ## 957        Shanghai Mainland China 31.20200 121.4491 2020-02-09     7 confirmed
    ## 958          Shanxi Mainland China 37.57770 112.2922 2020-02-09     4 confirmed
    ## 959         Sichuan Mainland China 30.61710 102.7103 2020-02-09    22 confirmed
    ## 961         Tianjin Mainland China 39.30540 117.3230 2020-02-09     3 confirmed
    ## 962        Xinjiang Mainland China 41.11290  85.2401 2020-02-09     3 confirmed
    ## 963          Yunnan Mainland China 24.97400 101.4870 2020-02-09     3 confirmed
    ## 964        Zhejiang Mainland China 29.18320 120.0934 2020-02-09    27 confirmed
    ## 965           Anhui Mainland China 31.82570 117.2264 2020-02-09     1     death
    ## 966           Gansu Mainland China 36.06110 103.8343 2020-02-09     1     death
    ## 967         Guangxi Mainland China 23.82980 108.7881 2020-02-09     1     death
    ## 968          Hainan Mainland China 19.19590 109.7453 2020-02-09     1     death
    ## 969           Hebei Mainland China 38.04280 114.5149 2020-02-09     1     death
    ## 970    Heilongjiang Mainland China 47.86200 127.7615 2020-02-09     1     death
    ## 971           Henan Mainland China 33.88202 113.6140 2020-02-09     2     death
    ## 972           Hubei Mainland China 30.97560 112.2707 2020-02-09    91     death
    ## 973        Shandong Mainland China 36.34270 118.1498 2020-02-09     1     death
    ## 975           Anhui Mainland China 31.82570 117.2264 2020-02-09    13 recovered
    ## 976         Beijing Mainland China 40.18240 116.4142 2020-02-09     3 recovered
    ## 977       Chongqing Mainland China 30.05720 107.8740 2020-02-09    12 recovered
    ## 979          Fujian Mainland China 26.07890 117.9874 2020-02-09    11 recovered
    ## 980           Gansu Mainland China 36.06110 103.8343 2020-02-09     4 recovered
    ## 981       Guangdong Mainland China 23.34170 113.4244 2020-02-09    29 recovered
    ## 982         Guangxi Mainland China 23.82980 108.7881 2020-02-09     1 recovered
    ## 983          Hainan Mainland China 19.19590 109.7453 2020-02-09     5 recovered
    ## 984           Hebei Mainland China 38.04280 114.5149 2020-02-09     4 recovered
    ## 985    Heilongjiang Mainland China 47.86200 127.7615 2020-02-09     1 recovered
    ## 986           Henan Mainland China 33.88202 113.6140 2020-02-09    37 recovered
    ## 987           Hubei Mainland China 30.97560 112.2707 2020-02-09   356 recovered
    ## 988           Hunan Mainland China 27.61040 111.7088 2020-02-09    30 recovered
    ## 989         Jiangsu Mainland China 32.97110 119.4550 2020-02-09    20 recovered
    ## 990         Jiangxi Mainland China 27.61400 115.7221 2020-02-09    18 recovered
    ## 991           Jilin Mainland China 43.66610 126.1923 2020-02-09     8 recovered
    ## 993        Liaoning Mainland China 41.29560 122.6085 2020-02-09     4 recovered
    ## 994         Ningxia Mainland China 37.26920 106.1655 2020-02-09    -2 recovered
    ## 995         Shaanxi Mainland China 35.19170 108.8701 2020-02-09     5 recovered
    ## 996        Shandong Mainland China 36.34270 118.1498 2020-02-09    19 recovered
    ## 997        Shanghai Mainland China 31.20200 121.4491 2020-02-09     3 recovered
    ## 998          Shanxi Mainland China 37.57770 112.2922 2020-02-09     4 recovered
    ## 999         Sichuan Mainland China 30.61710 102.7103 2020-02-09    11 recovered
    ## 1000         Yunnan Mainland China 24.97400 101.4870 2020-02-09     1 recovered
    ## 1001       Zhejiang Mainland China 29.18320 120.0934 2020-02-09    26 recovered
    ## 1008          Anhui Mainland China 31.82570 117.2264 2020-02-10    51 confirmed
    ## 1009        Beijing Mainland China 40.18240 116.4142 2020-02-10    11 confirmed
    ## 1010      Chongqing Mainland China 30.05720 107.8740 2020-02-10    18 confirmed
    ## 1012         Fujian Mainland China 26.07890 117.9874 2020-02-10    11 confirmed
    ## 1013      Guangdong Mainland China 23.34170 113.4244 2020-02-10    28 confirmed
    ## 1014        Guangxi Mainland China 23.82980 108.7881 2020-02-10    15 confirmed
    ## 1015        Guizhou Mainland China 26.81540 106.8748 2020-02-10    10 confirmed
    ## 1016         Hainan Mainland China 19.19590 109.7453 2020-02-10     7 confirmed
    ## 1017          Hebei Mainland China 38.04280 114.5149 2020-02-10    12 confirmed
    ## 1018   Heilongjiang Mainland China 47.86200 127.7615 2020-02-10    24 confirmed
    ## 1019          Henan Mainland China 33.88202 113.6140 2020-02-10    40 confirmed
    ## 1021          Hubei Mainland China 30.97560 112.2707 2020-02-10  2097 confirmed
    ## 1022          Hunan Mainland China 27.61040 111.7088 2020-02-10    41 confirmed
    ## 1023 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-10     4 confirmed
    ## 1024        Jiangsu Mainland China 32.97110 119.4550 2020-02-10    24 confirmed
    ## 1025        Jiangxi Mainland China 27.61400 115.7221 2020-02-10    31 confirmed
    ## 1026          Jilin Mainland China 43.66610 126.1923 2020-02-10     2 confirmed
    ## 1027       Liaoning Mainland China 41.29560 122.6085 2020-02-10     1 confirmed
    ## 1028        Ningxia Mainland China 37.26920 106.1655 2020-02-10     4 confirmed
    ## 1029        Shaanxi Mainland China 35.19170 108.8701 2020-02-10     5 confirmed
    ## 1030       Shandong Mainland China 36.34270 118.1498 2020-02-10    22 confirmed
    ## 1031       Shanghai Mainland China 31.20200 121.4491 2020-02-10     6 confirmed
    ## 1032        Sichuan Mainland China 30.61710 102.7103 2020-02-10    19 confirmed
    ## 1033        Tianjin Mainland China 39.30540 117.3230 2020-02-10     4 confirmed
    ## 1034       Xinjiang Mainland China 41.11290  85.2401 2020-02-10     4 confirmed
    ## 1035         Yunnan Mainland China 24.97400 101.4870 2020-02-10     8 confirmed
    ## 1036       Zhejiang Mainland China 29.18320 120.0934 2020-02-10    17 confirmed
    ## 1037          Anhui Mainland China 31.82570 117.2264 2020-02-10     2     death
    ## 1038   Heilongjiang Mainland China 47.86200 127.7615 2020-02-10     1     death
    ## 1039          Hubei Mainland China 30.97560 112.2707 2020-02-10   103     death
    ## 1040        Jiangxi Mainland China 27.61400 115.7221 2020-02-10     1     death
    ## 1042          Anhui Mainland China 31.82570 117.2264 2020-02-10    16 recovered
    ## 1043        Beijing Mainland China 40.18240 116.4142 2020-02-10     7 recovered
    ## 1044      Chongqing Mainland China 30.05720 107.8740 2020-02-10    15 recovered
    ## 1045         Fujian Mainland China 26.07890 117.9874 2020-02-10     4 recovered
    ## 1046          Gansu Mainland China 36.06110 103.8343 2020-02-10     1 recovered
    ## 1047      Guangdong Mainland China 23.34170 113.4244 2020-02-10    26 recovered
    ## 1048        Guangxi Mainland China 23.82980 108.7881 2020-02-10     6 recovered
    ## 1049        Guizhou Mainland China 26.81540 106.8748 2020-02-10     3 recovered
    ## 1050          Hebei Mainland China 38.04280 114.5149 2020-02-10     7 recovered
    ## 1051   Heilongjiang Mainland China 47.86200 127.7615 2020-02-10    16 recovered
    ## 1052          Henan Mainland China 33.88202 113.6140 2020-02-10    38 recovered
    ## 1053          Hubei Mainland China 30.97560 112.2707 2020-02-10   427 recovered
    ## 1054          Hunan Mainland China 27.61040 111.7088 2020-02-10    22 recovered
    ## 1055        Jiangsu Mainland China 32.97110 119.4550 2020-02-10    10 recovered
    ## 1056        Jiangxi Mainland China 27.61400 115.7221 2020-02-10    32 recovered
    ## 1057          Jilin Mainland China 43.66610 126.1923 2020-02-10     1 recovered
    ## 1058       Liaoning Mainland China 41.29560 122.6085 2020-02-10     1 recovered
    ## 1059        Shaanxi Mainland China 35.19170 108.8701 2020-02-10     5 recovered
    ## 1060       Shandong Mainland China 36.34270 118.1498 2020-02-10     3 recovered
    ## 1061       Shanghai Mainland China 31.20200 121.4491 2020-02-10     4 recovered
    ## 1062        Sichuan Mainland China 30.61710 102.7103 2020-02-10     9 recovered
    ## 1063        Tianjin Mainland China 39.30540 117.3230 2020-02-10     4 recovered
    ## 1064         Yunnan Mainland China 24.97400 101.4870 2020-02-10     1 recovered
    ## 1065       Zhejiang Mainland China 29.18320 120.0934 2020-02-10    41 recovered
    ## 1071          Anhui Mainland China 31.82570 117.2264 2020-02-11    30 confirmed
    ## 1072        Beijing Mainland China 40.18240 116.4142 2020-02-11     5 confirmed
    ## 1073      Chongqing Mainland China 30.05720 107.8740 2020-02-11    19 confirmed
    ## 1074         Fujian Mainland China 26.07890 117.9874 2020-02-11     6 confirmed
    ## 1075          Gansu Mainland China 36.06110 103.8343 2020-02-11     3 confirmed
    ## 1076      Guangdong Mainland China 23.34170 113.4244 2020-02-11    18 confirmed
    ## 1077        Guangxi Mainland China 23.82980 108.7881 2020-02-11     5 confirmed
    ## 1078        Guizhou Mainland China 26.81540 106.8748 2020-02-11    18 confirmed
    ## 1079         Hainan Mainland China 19.19590 109.7453 2020-02-11     6 confirmed
    ## 1080          Hebei Mainland China 38.04280 114.5149 2020-02-11    21 confirmed
    ## 1081   Heilongjiang Mainland China 47.86200 127.7615 2020-02-11    29 confirmed
    ## 1082          Henan Mainland China 33.88202 113.6140 2020-02-11    32 confirmed
    ## 1084          Hubei Mainland China 30.97560 112.2707 2020-02-11  1638 confirmed
    ## 1085          Hunan Mainland China 27.61040 111.7088 2020-02-11    33 confirmed
    ## 1086        Jiangsu Mainland China 32.97110 119.4550 2020-02-11    23 confirmed
    ## 1087        Jiangxi Mainland China 27.61400 115.7221 2020-02-11    33 confirmed
    ## 1088          Jilin Mainland China 43.66610 126.1923 2020-02-11     1 confirmed
    ## 1089       Liaoning Mainland China 41.29560 122.6085 2020-02-11     3 confirmed
    ## 1090        Ningxia Mainland China 37.26920 106.1655 2020-02-11     4 confirmed
    ## 1092        Shaanxi Mainland China 35.19170 108.8701 2020-02-11     6 confirmed
    ## 1093       Shandong Mainland China 36.34270 118.1498 2020-02-11    21 confirmed
    ## 1094       Shanghai Mainland China 31.20200 121.4491 2020-02-11     4 confirmed
    ## 1095         Shanxi Mainland China 37.57770 112.2922 2020-02-11     5 confirmed
    ## 1096        Sichuan Mainland China 30.61710 102.7103 2020-02-11    12 confirmed
    ## 1097        Tianjin Mainland China 39.30540 117.3230 2020-02-11    11 confirmed
    ## 1098       Xinjiang Mainland China 41.11290  85.2401 2020-02-11     6 confirmed
    ## 1099         Yunnan Mainland China 24.97400 101.4870 2020-02-11     4 confirmed
    ## 1100       Zhejiang Mainland China 29.18320 120.0934 2020-02-11    25 confirmed
    ## 1101          Anhui Mainland China 31.82570 117.2264 2020-02-11     1     death
    ## 1102        Beijing Mainland China 40.18240 116.4142 2020-02-11     1     death
    ## 1103      Chongqing Mainland China 30.05720 107.8740 2020-02-11     1     death
    ## 1104   Heilongjiang Mainland China 47.86200 127.7615 2020-02-11     1     death
    ## 1105          Henan Mainland China 33.88202 113.6140 2020-02-11     1     death
    ## 1106          Hubei Mainland China 30.97560 112.2707 2020-02-11    94     death
    ## 1107        Tianjin Mainland China 39.30540 117.3230 2020-02-11     1     death
    ## 1112          Anhui Mainland China 31.82570 117.2264 2020-02-11    17 recovered
    ## 1113        Beijing Mainland China 40.18240 116.4142 2020-02-11     4 recovered
    ## 1114      Chongqing Mainland China 30.05720 107.8740 2020-02-11    13 recovered
    ## 1115         Fujian Mainland China 26.07890 117.9874 2020-02-11     6 recovered
    ## 1116          Gansu Mainland China 36.06110 103.8343 2020-02-11     7 recovered
    ## 1117      Guangdong Mainland China 23.34170 113.4244 2020-02-11    45 recovered
    ## 1118        Guangxi Mainland China 23.82980 108.7881 2020-02-11     9 recovered
    ## 1119        Guizhou Mainland China 26.81540 106.8748 2020-02-11     7 recovered
    ## 1120         Hainan Mainland China 19.19590 109.7453 2020-02-11     1 recovered
    ## 1121          Hebei Mainland China 38.04280 114.5149 2020-02-11     7 recovered
    ## 1122   Heilongjiang Mainland China 47.86200 127.7615 2020-02-11    -2 recovered
    ## 1123          Henan Mainland China 33.88202 113.6140 2020-02-11    27 recovered
    ## 1124          Hubei Mainland China 30.97560 112.2707 2020-02-11   417 recovered
    ## 1125          Hunan Mainland China 27.61040 111.7088 2020-02-11    39 recovered
    ## 1126        Jiangsu Mainland China 32.97110 119.4550 2020-02-11    12 recovered
    ## 1127        Jiangxi Mainland China 27.61400 115.7221 2020-02-11    23 recovered
    ## 1128          Jilin Mainland China 43.66610 126.1923 2020-02-11     5 recovered
    ## 1129       Liaoning Mainland China 41.29560 122.6085 2020-02-11     6 recovered
    ## 1130        Ningxia Mainland China 37.26920 106.1655 2020-02-11     9 recovered
    ## 1131        Qinghai Mainland China 35.74520  95.9956 2020-02-11     2 recovered
    ## 1132        Shaanxi Mainland China 35.19170 108.8701 2020-02-11     2 recovered
    ## 1133       Shandong Mainland China 36.34270 118.1498 2020-02-11    14 recovered
    ## 1134       Shanghai Mainland China 31.20200 121.4491 2020-02-11     4 recovered
    ## 1135         Shanxi Mainland China 37.57770 112.2922 2020-02-11     5 recovered
    ## 1136        Sichuan Mainland China 30.61710 102.7103 2020-02-11     5 recovered
    ## 1137        Tianjin Mainland China 39.30540 117.3230 2020-02-11     2 recovered
    ## 1138       Xinjiang Mainland China 41.11290  85.2401 2020-02-11     3 recovered
    ## 1139         Yunnan Mainland China 24.97400 101.4870 2020-02-11     1 recovered
    ## 1140       Zhejiang Mainland China 29.18320 120.0934 2020-02-11    28 recovered
    ## 1144          Anhui Mainland China 31.82570 117.2264 2020-02-12    29 confirmed
    ## 1145        Beijing Mainland China 40.18240 116.4142 2020-02-12    10 confirmed
    ## 1146      Chongqing Mainland China 30.05720 107.8740 2020-02-12    13 confirmed
    ## 1148         Fujian Mainland China 26.07890 117.9874 2020-02-12     5 confirmed
    ## 1149          Gansu Mainland China 36.06110 103.8343 2020-02-12     1 confirmed
    ## 1150      Guangdong Mainland China 23.34170 113.4244 2020-02-12    42 confirmed
    ## 1151        Guangxi Mainland China 23.82980 108.7881 2020-02-12     7 confirmed
    ## 1152        Guizhou Mainland China 26.81540 106.8748 2020-02-12     6 confirmed
    ## 1153         Hainan Mainland China 19.19590 109.7453 2020-02-12    13 confirmed
    ## 1154          Hebei Mainland China 38.04280 114.5149 2020-02-12    12 confirmed
    ## 1155   Heilongjiang Mainland China 47.86200 127.7615 2020-02-12    18 confirmed
    ## 1156          Henan Mainland China 33.88202 113.6140 2020-02-12    30 confirmed
    ## 1158          Hunan Mainland China 27.61040 111.7088 2020-02-12    34 confirmed
    ## 1159 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-12     2 confirmed
    ## 1160        Jiangsu Mainland China 32.97110 119.4550 2020-02-12    28 confirmed
    ## 1161        Jiangxi Mainland China 27.61400 115.7221 2020-02-12    40 confirmed
    ## 1162          Jilin Mainland China 43.66610 126.1923 2020-02-12     2 confirmed
    ## 1163       Liaoning Mainland China 41.29560 122.6085 2020-02-12     5 confirmed
    ## 1164        Ningxia Mainland China 37.26920 106.1655 2020-02-12     5 confirmed
    ## 1165        Shaanxi Mainland China 35.19170 108.8701 2020-02-12     6 confirmed
    ## 1166       Shandong Mainland China 36.34270 118.1498 2020-02-12    10 confirmed
    ## 1167       Shanghai Mainland China 31.20200 121.4491 2020-02-12     8 confirmed
    ## 1168         Shanxi Mainland China 37.57770 112.2922 2020-02-12     2 confirmed
    ## 1169        Sichuan Mainland China 30.61710 102.7103 2020-02-12    19 confirmed
    ## 1170        Tianjin Mainland China 39.30540 117.3230 2020-02-12     6 confirmed
    ## 1171       Xinjiang Mainland China 41.11290  85.2401 2020-02-12     4 confirmed
    ## 1172         Yunnan Mainland China 24.97400 101.4870 2020-02-12     1 confirmed
    ## 1173       Zhejiang Mainland China 29.18320 120.0934 2020-02-12    14 confirmed
    ## 1174         Hainan Mainland China 19.19590 109.7453 2020-02-12     1     death
    ## 1175          Henan Mainland China 33.88202 113.6140 2020-02-12     1     death
    ## 1176          Hunan Mainland China 27.61040 111.7088 2020-02-12     1     death
    ## 1177       Liaoning Mainland China 41.29560 122.6085 2020-02-12     1     death
    ## 1178       Shandong Mainland China 36.34270 118.1498 2020-02-12     1     death
    ## 1189          Anhui Mainland China 31.82570 117.2264 2020-02-12    22 recovered
    ## 1190        Beijing Mainland China 40.18240 116.4142 2020-02-12     8 recovered
    ## 1191      Chongqing Mainland China 30.05720 107.8740 2020-02-12    23 recovered
    ## 1192         Fujian Mainland China 26.07890 117.9874 2020-02-12     8 recovered
    ## 1193          Gansu Mainland China 36.06110 103.8343 2020-02-12     7 recovered
    ## 1194      Guangdong Mainland China 23.34170 113.4244 2020-02-12    63 recovered
    ## 1195        Guangxi Mainland China 23.82980 108.7881 2020-02-12    -1 recovered
    ## 1196        Guizhou Mainland China 26.81540 106.8748 2020-02-12     1 recovered
    ## 1197         Hainan Mainland China 19.19590 109.7453 2020-02-12     7 recovered
    ## 1198          Hebei Mainland China 38.04280 114.5149 2020-02-12     6 recovered
    ## 1199   Heilongjiang Mainland China 47.86200 127.7615 2020-02-12     3 recovered
    ## 1200          Henan Mainland China 33.88202 113.6140 2020-02-12    28 recovered
    ## 1202          Hubei Mainland China 30.97560 112.2707 2020-02-12    47 recovered
    ## 1203          Hunan Mainland China 27.61040 111.7088 2020-02-12    57 recovered
    ## 1204 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-12     1 recovered
    ## 1205        Jiangsu Mainland China 32.97110 119.4550 2020-02-12    32 recovered
    ## 1206        Jiangxi Mainland China 27.61400 115.7221 2020-02-12    24 recovered
    ## 1207          Jilin Mainland China 43.66610 126.1923 2020-02-12     4 recovered
    ## 1208       Liaoning Mainland China 41.29560 122.6085 2020-02-12     1 recovered
    ## 1211        Ningxia Mainland China 37.26920 106.1655 2020-02-12     2 recovered
    ## 1212        Qinghai Mainland China 35.74520  95.9956 2020-02-12     4 recovered
    ## 1213        Shaanxi Mainland China 35.19170 108.8701 2020-02-12    11 recovered
    ## 1214       Shandong Mainland China 36.34270 118.1498 2020-02-12    12 recovered
    ## 1215       Shanghai Mainland China 31.20200 121.4491 2020-02-12     5 recovered
    ## 1216         Shanxi Mainland China 37.57770 112.2922 2020-02-12     3 recovered
    ## 1217        Sichuan Mainland China 30.61710 102.7103 2020-02-12     7 recovered
    ## 1218        Tianjin Mainland China 39.30540 117.3230 2020-02-12     1 recovered
    ## 1219          Tibet Mainland China 31.69270  88.0924 2020-02-12     1 recovered
    ## 1220         Yunnan Mainland China 24.97400 101.4870 2020-02-12     6 recovered
    ## 1221       Zhejiang Mainland China 29.18320 120.0934 2020-02-12    51 recovered
    ## 1225          Anhui Mainland China 31.82570 117.2264 2020-02-13    21 confirmed
    ## 1226        Beijing Mainland China 40.18240 116.4142 2020-02-13    14 confirmed
    ## 1227      Chongqing Mainland China 30.05720 107.8740 2020-02-13    11 confirmed
    ## 1228         Fujian Mainland China 26.07890 117.9874 2020-02-13     7 confirmed
    ## 1229          Gansu Mainland China 36.06110 103.8343 2020-02-13     3 confirmed
    ## 1230      Guangdong Mainland China 23.34170 113.4244 2020-02-13    22 confirmed
    ## 1231        Guizhou Mainland China 26.81540 106.8748 2020-02-13     2 confirmed
    ## 1232          Hebei Mainland China 38.04280 114.5149 2020-02-13    14 confirmed
    ## 1233   Heilongjiang Mainland China 47.86200 127.7615 2020-02-13    17 confirmed
    ## 1234          Henan Mainland China 33.88202 113.6140 2020-02-13    34 confirmed
    ## 1236          Hubei Mainland China 30.97560 112.2707 2020-02-13 14840 confirmed
    ## 1237          Hunan Mainland China 27.61040 111.7088 2020-02-13    22 confirmed
    ## 1238 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-13     1 confirmed
    ## 1239        Jiangsu Mainland China 32.97110 119.4550 2020-02-13    27 confirmed
    ## 1240        Jiangxi Mainland China 27.61400 115.7221 2020-02-13    28 confirmed
    ## 1241          Jilin Mainland China 43.66610 126.1923 2020-02-13     1 confirmed
    ## 1242       Liaoning Mainland China 41.29560 122.6085 2020-02-13     1 confirmed
    ## 1243        Ningxia Mainland China 37.26920 106.1655 2020-02-13     6 confirmed
    ## 1246        Shaanxi Mainland China 35.19170 108.8701 2020-02-13     4 confirmed
    ## 1247       Shandong Mainland China 36.34270 118.1498 2020-02-13    12 confirmed
    ## 1248       Shanghai Mainland China 31.20200 121.4491 2020-02-13     4 confirmed
    ## 1249        Sichuan Mainland China 30.61710 102.7103 2020-02-13    15 confirmed
    ## 1250        Tianjin Mainland China 39.30540 117.3230 2020-02-13     7 confirmed
    ## 1251       Xinjiang Mainland China 41.11290  85.2401 2020-02-13     4 confirmed
    ## 1252         Yunnan Mainland China 24.97400 101.4870 2020-02-13     2 confirmed
    ## 1253       Zhejiang Mainland China 29.18320 120.0934 2020-02-13    14 confirmed
    ## 1255          Anhui Mainland China 31.82570 117.2264 2020-02-13     1     death
    ## 1256      Chongqing Mainland China 30.05720 107.8740 2020-02-13     1     death
    ## 1257      Guangdong Mainland China 23.34170 113.4244 2020-02-13     1     death
    ## 1258        Guangxi Mainland China 23.82980 108.7881 2020-02-13     1     death
    ## 1259          Hebei Mainland China 38.04280 114.5149 2020-02-13     1     death
    ## 1260   Heilongjiang Mainland China 47.86200 127.7615 2020-02-13     1     death
    ## 1261          Henan Mainland China 33.88202 113.6140 2020-02-13     2     death
    ## 1262          Hubei Mainland China 30.97560 112.2707 2020-02-13   242     death
    ## 1263        Tianjin Mainland China 39.30540 117.3230 2020-02-13     1     death
    ## 1264       Xinjiang Mainland China 41.11290  85.2401 2020-02-13     1     death
    ## 1268          Anhui Mainland China 31.82570 117.2264 2020-02-13    30 recovered
    ## 1269        Beijing Mainland China 40.18240 116.4142 2020-02-13    13 recovered
    ## 1270      Chongqing Mainland China 30.05720 107.8740 2020-02-13    26 recovered
    ## 1271         Fujian Mainland China 26.07890 117.9874 2020-02-13     4 recovered
    ## 1272          Gansu Mainland China 36.06110 103.8343 2020-02-13     8 recovered
    ## 1273      Guangdong Mainland China 23.34170 113.4244 2020-02-13    39 recovered
    ## 1274        Guangxi Mainland China 23.82980 108.7881 2020-02-13     1 recovered
    ## 1275        Guizhou Mainland China 26.81540 106.8748 2020-02-13     9 recovered
    ## 1276         Hainan Mainland China 19.19590 109.7453 2020-02-13     3 recovered
    ## 1277          Hebei Mainland China 38.04280 114.5149 2020-02-13    14 recovered
    ## 1278   Heilongjiang Mainland China 47.86200 127.7615 2020-02-13     2 recovered
    ## 1279          Henan Mainland China 33.88202 113.6140 2020-02-13    50 recovered
    ## 1280          Hubei Mainland China 30.97560 112.2707 2020-02-13   773 recovered
    ## 1281          Hunan Mainland China 27.61040 111.7088 2020-02-13    35 recovered
    ## 1282        Jiangsu Mainland China 32.97110 119.4550 2020-02-13    14 recovered
    ## 1283        Jiangxi Mainland China 27.61400 115.7221 2020-02-13    18 recovered
    ## 1284          Jilin Mainland China 43.66610 126.1923 2020-02-13     2 recovered
    ## 1285       Liaoning Mainland China 41.29560 122.6085 2020-02-13     2 recovered
    ## 1288        Qinghai Mainland China 35.74520  95.9956 2020-02-13     2 recovered
    ## 1289        Shaanxi Mainland China 35.19170 108.8701 2020-02-13     3 recovered
    ## 1290       Shandong Mainland China 36.34270 118.1498 2020-02-13    13 recovered
    ## 1291       Shanghai Mainland China 31.20200 121.4491 2020-02-13     5 recovered
    ## 1292         Shanxi Mainland China 37.57770 112.2922 2020-02-13     3 recovered
    ## 1293        Sichuan Mainland China 30.61710 102.7103 2020-02-13    12 recovered
    ## 1294        Tianjin Mainland China 39.30540 117.3230 2020-02-13    10 recovered
    ## 1296       Xinjiang Mainland China 41.11290  85.2401 2020-02-13     3 recovered
    ## 1297         Yunnan Mainland China 24.97400 101.4870 2020-02-13     1 recovered
    ## 1298       Zhejiang Mainland China 29.18320 120.0934 2020-02-13    39 recovered
    ## 1302          Anhui Mainland China 31.82570 117.2264 2020-02-14    24 confirmed
    ## 1303        Beijing Mainland China 40.18240 116.4142 2020-02-14     6 confirmed
    ## 1304      Chongqing Mainland China 30.05720 107.8740 2020-02-14     8 confirmed
    ## 1306         Fujian Mainland China 26.07890 117.9874 2020-02-14     2 confirmed
    ## 1307      Guangdong Mainland China 23.34170 113.4244 2020-02-14    20 confirmed
    ## 1308        Guangxi Mainland China 23.82980 108.7881 2020-02-14     4 confirmed
    ## 1309        Guizhou Mainland China 26.81540 106.8748 2020-02-14     5 confirmed
    ## 1310         Hainan Mainland China 19.19590 109.7453 2020-02-14     2 confirmed
    ## 1311          Hebei Mainland China 38.04280 114.5149 2020-02-14    18 confirmed
    ## 1312   Heilongjiang Mainland China 47.86200 127.7615 2020-02-14    24 confirmed
    ## 1313          Henan Mainland China 33.88202 113.6140 2020-02-14    15 confirmed
    ## 1315          Hubei Mainland China 30.97560 112.2707 2020-02-14  6200 confirmed
    ## 1316          Hunan Mainland China 27.61040 111.7088 2020-02-14    20 confirmed
    ## 1317 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-14     4 confirmed
    ## 1318        Jiangsu Mainland China 32.97110 119.4550 2020-02-14    23 confirmed
    ## 1319        Jiangxi Mainland China 27.61400 115.7221 2020-02-14    28 confirmed
    ## 1320          Jilin Mainland China 43.66610 126.1923 2020-02-14     2 confirmed
    ## 1321       Liaoning Mainland China 41.29560 122.6085 2020-02-14     2 confirmed
    ## 1322        Ningxia Mainland China 37.26920 106.1655 2020-02-14     3 confirmed
    ## 1323        Shaanxi Mainland China 35.19170 108.8701 2020-02-14     1 confirmed
    ## 1324       Shandong Mainland China 36.34270 118.1498 2020-02-14    14 confirmed
    ## 1325       Shanghai Mainland China 31.20200 121.4491 2020-02-14     3 confirmed
    ## 1326         Shanxi Mainland China 37.57770 112.2922 2020-02-14     1 confirmed
    ## 1327        Sichuan Mainland China 30.61710 102.7103 2020-02-14    12 confirmed
    ## 1328        Tianjin Mainland China 39.30540 117.3230 2020-02-14     1 confirmed
    ## 1329       Xinjiang Mainland China 41.11290  85.2401 2020-02-14     2 confirmed
    ## 1330         Yunnan Mainland China 24.97400 101.4870 2020-02-14     6 confirmed
    ## 1331       Zhejiang Mainland China 29.18320 120.0934 2020-02-14    10 confirmed
    ## 1332          Anhui Mainland China 31.82570 117.2264 2020-02-14     1     death
    ## 1333      Chongqing Mainland China 30.05720 107.8740 2020-02-14     1     death
    ## 1334   Heilongjiang Mainland China 47.86200 127.7615 2020-02-14     2     death
    ## 1335          Henan Mainland China 33.88202 113.6140 2020-02-14     1     death
    ## 1336          Hubei Mainland China 30.97560 112.2707 2020-02-14   147     death
    ## 1338          Anhui Mainland China 31.82570 117.2264 2020-02-14    36 recovered
    ## 1339        Beijing Mainland China 40.18240 116.4142 2020-02-14    11 recovered
    ## 1340      Chongqing Mainland China 30.05720 107.8740 2020-02-14    24 recovered
    ## 1341         Fujian Mainland China 26.07890 117.9874 2020-02-14     6 recovered
    ## 1342      Guangdong Mainland China 23.34170 113.4244 2020-02-14    48 recovered
    ## 1343        Guangxi Mainland China 23.82980 108.7881 2020-02-14     3 recovered
    ## 1344        Guizhou Mainland China 26.81540 106.8748 2020-02-14     1 recovered
    ## 1345         Hainan Mainland China 19.19590 109.7453 2020-02-14    13 recovered
    ## 1346          Hebei Mainland China 38.04280 114.5149 2020-02-14    19 recovered
    ## 1347   Heilongjiang Mainland China 47.86200 127.7615 2020-02-14    14 recovered
    ## 1348          Henan Mainland China 33.88202 113.6140 2020-02-14    61 recovered
    ## 1349          Hubei Mainland China 30.97560 112.2707 2020-02-14  1315 recovered
    ## 1350          Hunan Mainland China 27.61040 111.7088 2020-02-14    25 recovered
    ## 1351        Jiangsu Mainland China 32.97110 119.4550 2020-02-14    18 recovered
    ## 1352        Jiangxi Mainland China 27.61400 115.7221 2020-02-14    17 recovered
    ## 1353          Jilin Mainland China 43.66610 126.1923 2020-02-14     1 recovered
    ## 1354       Liaoning Mainland China 41.29560 122.6085 2020-02-14     7 recovered
    ## 1355        Shaanxi Mainland China 35.19170 108.8701 2020-02-14     8 recovered
    ## 1356       Shandong Mainland China 36.34270 118.1498 2020-02-14    31 recovered
    ## 1357       Shanghai Mainland China 31.20200 121.4491 2020-02-14    28 recovered
    ## 1358         Shanxi Mainland China 37.57770 112.2922 2020-02-14     2 recovered
    ## 1359        Sichuan Mainland China 30.61710 102.7103 2020-02-14    10 recovered
    ## 1361        Tianjin Mainland China 39.30540 117.3230 2020-02-14    10 recovered
    ## 1362         Yunnan Mainland China 24.97400 101.4870 2020-02-14     9 recovered
    ## 1363       Zhejiang Mainland China 29.18320 120.0934 2020-02-14    43 recovered
    ## 1368          Anhui Mainland China 31.82570 117.2264 2020-02-15    16 confirmed
    ## 1369        Beijing Mainland China 40.18240 116.4142 2020-02-15     3 confirmed
    ## 1370      Chongqing Mainland China 30.05720 107.8740 2020-02-15     7 confirmed
    ## 1372         Fujian Mainland China 26.07890 117.9874 2020-02-15     4 confirmed
    ## 1373      Guangdong Mainland China 23.34170 113.4244 2020-02-15    33 confirmed
    ## 1374        Guangxi Mainland China 23.82980 108.7881 2020-02-15     9 confirmed
    ## 1375        Guizhou Mainland China 26.81540 106.8748 2020-02-15     3 confirmed
    ## 1376         Hainan Mainland China 19.19590 109.7453 2020-02-15     3 confirmed
    ## 1377          Hebei Mainland China 38.04280 114.5149 2020-02-15     8 confirmed
    ## 1378   Heilongjiang Mainland China 47.86200 127.7615 2020-02-15     6 confirmed
    ## 1379          Henan Mainland China 33.88202 113.6140 2020-02-15    28 confirmed
    ## 1380          Hubei Mainland China 30.97560 112.2707 2020-02-15  1843 confirmed
    ## 1381          Hunan Mainland China 27.61040 111.7088 2020-02-15    13 confirmed
    ## 1382 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-15     3 confirmed
    ## 1383        Jiangsu Mainland China 32.97110 119.4550 2020-02-15    11 confirmed
    ## 1384        Jiangxi Mainland China 27.61400 115.7221 2020-02-15    13 confirmed
    ## 1385          Jilin Mainland China 43.66610 126.1923 2020-02-15     2 confirmed
    ## 1386        Ningxia Mainland China 37.26920 106.1655 2020-02-15     3 confirmed
    ## 1387        Shaanxi Mainland China 35.19170 108.8701 2020-02-15     2 confirmed
    ## 1388       Shandong Mainland China 36.34270 118.1498 2020-02-15     9 confirmed
    ## 1389       Shanghai Mainland China 31.20200 121.4491 2020-02-15     8 confirmed
    ## 1390         Shanxi Mainland China 37.57770 112.2922 2020-02-15     1 confirmed
    ## 1391        Sichuan Mainland China 30.61710 102.7103 2020-02-15     7 confirmed
    ## 1392        Tianjin Mainland China 39.30540 117.3230 2020-02-15     2 confirmed
    ## 1393       Xinjiang Mainland China 41.11290  85.2401 2020-02-15     5 confirmed
    ## 1394         Yunnan Mainland China 24.97400 101.4870 2020-02-15     6 confirmed
    ## 1395       Zhejiang Mainland China 29.18320 120.0934 2020-02-15     7 confirmed
    ## 1397        Beijing Mainland China 40.18240 116.4142 2020-02-15     1     death
    ## 1398          Henan Mainland China 33.88202 113.6140 2020-02-15     2     death
    ## 1399          Hubei Mainland China 30.97560 112.2707 2020-02-15   139     death
    ## 1407          Anhui Mainland China 31.82570 117.2264 2020-02-15    28 recovered
    ## 1408        Beijing Mainland China 40.18240 116.4142 2020-02-15    18 recovered
    ## 1409      Chongqing Mainland China 30.05720 107.8740 2020-02-15    32 recovered
    ## 1410         Fujian Mainland China 26.07890 117.9874 2020-02-15     8 recovered
    ## 1411          Gansu Mainland China 36.06110 103.8343 2020-02-15    10 recovered
    ## 1412      Guangdong Mainland China 23.34170 113.4244 2020-02-15    48 recovered
    ## 1413        Guangxi Mainland China 23.82980 108.7881 2020-02-15     8 recovered
    ## 1414        Guizhou Mainland China 26.81540 106.8748 2020-02-15    13 recovered
    ## 1415         Hainan Mainland China 19.19590 109.7453 2020-02-15    -4 recovered
    ## 1416          Hebei Mainland China 38.04280 114.5149 2020-02-15    14 recovered
    ## 1417   Heilongjiang Mainland China 47.86200 127.7615 2020-02-15    21 recovered
    ## 1418          Henan Mainland China 33.88202 113.6140 2020-02-15    34 recovered
    ## 1419          Hubei Mainland China 30.97560 112.2707 2020-02-15   849 recovered
    ## 1420          Hunan Mainland China 27.61040 111.7088 2020-02-15    61 recovered
    ## 1421 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-15     1 recovered
    ## 1422        Jiangsu Mainland China 32.97110 119.4550 2020-02-15    29 recovered
    ## 1423        Jiangxi Mainland China 27.61400 115.7221 2020-02-15    23 recovered
    ## 1424          Jilin Mainland China 43.66610 126.1923 2020-02-15     1 recovered
    ## 1425       Liaoning Mainland China 41.29560 122.6085 2020-02-15     2 recovered
    ## 1426        Ningxia Mainland China 37.26920 106.1655 2020-02-15     9 recovered
    ## 1427        Qinghai Mainland China 35.74520  95.9956 2020-02-15     2 recovered
    ## 1428        Shaanxi Mainland China 35.19170 108.8701 2020-02-15     6 recovered
    ## 1429       Shandong Mainland China 36.34270 118.1498 2020-02-15    20 recovered
    ## 1430       Shanghai Mainland China 31.20200 121.4491 2020-02-15    34 recovered
    ## 1431         Shanxi Mainland China 37.57770 112.2922 2020-02-15     8 recovered
    ## 1432        Sichuan Mainland China 30.61710 102.7103 2020-02-15     5 recovered
    ## 1433        Tianjin Mainland China 39.30540 117.3230 2020-02-15     6 recovered
    ## 1434       Xinjiang Mainland China 41.11290  85.2401 2020-02-15     4 recovered
    ## 1435         Yunnan Mainland China 24.97400 101.4870 2020-02-15     6 recovered
    ## 1436       Zhejiang Mainland China 29.18320 120.0934 2020-02-15    25 recovered
    ## 1442          Anhui Mainland China 31.82570 117.2264 2020-02-16    12 confirmed
    ## 1443        Beijing Mainland China 40.18240 116.4142 2020-02-16     5 confirmed
    ## 1444      Chongqing Mainland China 30.05720 107.8740 2020-02-16     7 confirmed
    ## 1446         Fujian Mainland China 26.07890 117.9874 2020-02-16     2 confirmed
    ## 1447      Guangdong Mainland China 23.34170 113.4244 2020-02-16    22 confirmed
    ## 1448        Guangxi Mainland China 23.82980 108.7881 2020-02-16     2 confirmed
    ## 1449        Guizhou Mainland China 26.81540 106.8748 2020-02-16     1 confirmed
    ## 1450          Hebei Mainland China 38.04280 114.5149 2020-02-16     9 confirmed
    ## 1451   Heilongjiang Mainland China 47.86200 127.7615 2020-02-16    20 confirmed
    ## 1452          Henan Mainland China 33.88202 113.6140 2020-02-16    19 confirmed
    ## 1454          Hubei Mainland China 30.97560 112.2707 2020-02-16  1933 confirmed
    ## 1455          Hunan Mainland China 27.61040 111.7088 2020-02-16     3 confirmed
    ## 1456 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-16     2 confirmed
    ## 1457        Jiangsu Mainland China 32.97110 119.4550 2020-02-16    13 confirmed
    ## 1458        Jiangxi Mainland China 27.61400 115.7221 2020-02-16    12 confirmed
    ## 1459          Jilin Mainland China 43.66610 126.1923 2020-02-16     1 confirmed
    ## 1460       Liaoning Mainland China 41.29560 122.6085 2020-02-16     2 confirmed
    ## 1461        Shaanxi Mainland China 35.19170 108.8701 2020-02-16     4 confirmed
    ## 1462       Shandong Mainland China 36.34270 118.1498 2020-02-16     5 confirmed
    ## 1463       Shanghai Mainland China 31.20200 121.4491 2020-02-16     2 confirmed
    ## 1464         Shanxi Mainland China 37.57770 112.2922 2020-02-16     1 confirmed
    ## 1465        Sichuan Mainland China 30.61710 102.7103 2020-02-16    11 confirmed
    ## 1467        Tianjin Mainland China 39.30540 117.3230 2020-02-16     2 confirmed
    ## 1468       Xinjiang Mainland China 41.11290  85.2401 2020-02-16     1 confirmed
    ## 1469         Yunnan Mainland China 24.97400 101.4870 2020-02-16     3 confirmed
    ## 1470       Zhejiang Mainland China 29.18320 120.0934 2020-02-16     5 confirmed
    ## 1471          Hubei Mainland China 30.97560 112.2707 2020-02-16   100     death
    ## 1472          Hunan Mainland China 27.61040 111.7088 2020-02-16     1     death
    ## 1473        Sichuan Mainland China 30.61710 102.7103 2020-02-16     2     death
    ## 1479          Anhui Mainland China 31.82570 117.2264 2020-02-16    34 recovered
    ## 1480        Beijing Mainland China 40.18240 116.4142 2020-02-16    10 recovered
    ## 1481      Chongqing Mainland China 30.05720 107.8740 2020-02-16    23 recovered
    ## 1482         Fujian Mainland China 26.07890 117.9874 2020-02-16    11 recovered
    ## 1483          Gansu Mainland China 36.06110 103.8343 2020-02-16     5 recovered
    ## 1484      Guangdong Mainland China 23.34170 113.4244 2020-02-16    55 recovered
    ## 1485        Guangxi Mainland China 23.82980 108.7881 2020-02-16     5 recovered
    ## 1486        Guizhou Mainland China 26.81540 106.8748 2020-02-16     5 recovered
    ## 1487         Hainan Mainland China 19.19590 109.7453 2020-02-16    13 recovered
    ## 1488          Hebei Mainland China 38.04280 114.5149 2020-02-16     4 recovered
    ## 1489   Heilongjiang Mainland China 47.86200 127.7615 2020-02-16    11 recovered
    ## 1490          Henan Mainland China 33.88202 113.6140 2020-02-16    49 recovered
    ## 1492          Hubei Mainland China 30.97560 112.2707 2020-02-16  1016 recovered
    ## 1493          Hunan Mainland China 27.61040 111.7088 2020-02-16    39 recovered
    ## 1494 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-16     1 recovered
    ## 1495        Jiangsu Mainland China 32.97110 119.4550 2020-02-16    32 recovered
    ## 1496        Jiangxi Mainland China 27.61400 115.7221 2020-02-16    30 recovered
    ## 1497          Jilin Mainland China 43.66610 126.1923 2020-02-16     4 recovered
    ## 1498       Liaoning Mainland China 41.29560 122.6085 2020-02-16     9 recovered
    ## 1500        Shaanxi Mainland China 35.19170 108.8701 2020-02-16    11 recovered
    ## 1501       Shandong Mainland China 36.34270 118.1498 2020-02-16    17 recovered
    ## 1502       Shanghai Mainland China 31.20200 121.4491 2020-02-16    16 recovered
    ## 1503         Shanxi Mainland China 37.57770 112.2922 2020-02-16     4 recovered
    ## 1504        Sichuan Mainland China 30.61710 102.7103 2020-02-16    12 recovered
    ## 1505        Tianjin Mainland China 39.30540 117.3230 2020-02-16     8 recovered
    ## 1506       Xinjiang Mainland China 41.11290  85.2401 2020-02-16     2 recovered
    ## 1507       Zhejiang Mainland China 29.18320 120.0934 2020-02-16    28 recovered
    ## 1512          Anhui Mainland China 31.82570 117.2264 2020-02-17    11 confirmed
    ## 1513        Beijing Mainland China 40.18240 116.4142 2020-02-17     1 confirmed
    ## 1515      Chongqing Mainland China 30.05720 107.8740 2020-02-17     2 confirmed
    ## 1517         Fujian Mainland China 26.07890 117.9874 2020-02-17     3 confirmed
    ## 1518          Gansu Mainland China 36.06110 103.8343 2020-02-17     1 confirmed
    ## 1519      Guangdong Mainland China 23.34170 113.4244 2020-02-17     6 confirmed
    ## 1520        Guangxi Mainland China 23.82980 108.7881 2020-02-17     1 confirmed
    ## 1521        Guizhou Mainland China 26.81540 106.8748 2020-02-17     2 confirmed
    ## 1522         Hainan Mainland China 19.19590 109.7453 2020-02-17     1 confirmed
    ## 1523          Hebei Mainland China 38.04280 114.5149 2020-02-17     1 confirmed
    ## 1524   Heilongjiang Mainland China 47.86200 127.7615 2020-02-17    12 confirmed
    ## 1525          Henan Mainland China 33.88202 113.6140 2020-02-17    15 confirmed
    ## 1527          Hubei Mainland China 30.97560 112.2707 2020-02-17  1807 confirmed
    ## 1528          Hunan Mainland China 27.61040 111.7088 2020-02-17     2 confirmed
    ## 1529 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-17     2 confirmed
    ## 1530        Jiangsu Mainland China 32.97110 119.4550 2020-02-17     9 confirmed
    ## 1531        Jiangxi Mainland China 27.61400 115.7221 2020-02-17     5 confirmed
    ## 1532        Shaanxi Mainland China 35.19170 108.8701 2020-02-17     4 confirmed
    ## 1533       Shandong Mainland China 36.34270 118.1498 2020-02-17     4 confirmed
    ## 1534       Shanghai Mainland China 31.20200 121.4491 2020-02-17     5 confirmed
    ## 1535         Shanxi Mainland China 37.57770 112.2922 2020-02-17     1 confirmed
    ## 1536        Sichuan Mainland China 30.61710 102.7103 2020-02-17    14 confirmed
    ## 1538        Tianjin Mainland China 39.30540 117.3230 2020-02-17     1 confirmed
    ## 1539       Xinjiang Mainland China 41.11290  85.2401 2020-02-17     4 confirmed
    ## 1540       Zhejiang Mainland China 29.18320 120.0934 2020-02-17     4 confirmed
    ## 1541      Guangdong Mainland China 23.34170 113.4244 2020-02-17     2     death
    ## 1542          Henan Mainland China 33.88202 113.6140 2020-02-17     3     death
    ## 1543          Hubei Mainland China 30.97560 112.2707 2020-02-17    93     death
    ## 1548          Anhui Mainland China 31.82570 117.2264 2020-02-17    25 recovered
    ## 1549        Beijing Mainland China 40.18240 116.4142 2020-02-17     6 recovered
    ## 1550      Chongqing Mainland China 30.05720 107.8740 2020-02-17    18 recovered
    ## 1551         Fujian Mainland China 26.07890 117.9874 2020-02-17     8 recovered
    ## 1552          Gansu Mainland China 36.06110 103.8343 2020-02-17     4 recovered
    ## 1553      Guangdong Mainland China 23.34170 113.4244 2020-02-17    59 recovered
    ## 1554        Guangxi Mainland China 23.82980 108.7881 2020-02-17     4 recovered
    ## 1555        Guizhou Mainland China 26.81540 106.8748 2020-02-17    11 recovered
    ## 1556         Hainan Mainland China 19.19590 109.7453 2020-02-17     7 recovered
    ## 1557          Hebei Mainland China 38.04280 114.5149 2020-02-17    17 recovered
    ## 1558   Heilongjiang Mainland China 47.86200 127.7615 2020-02-17     6 recovered
    ## 1559          Henan Mainland China 33.88202 113.6140 2020-02-17    69 recovered
    ## 1560          Hubei Mainland China 30.97560 112.2707 2020-02-17  1223 recovered
    ## 1561          Hunan Mainland China 27.61040 111.7088 2020-02-17    34 recovered
    ## 1562        Jiangsu Mainland China 32.97110 119.4550 2020-02-17    40 recovered
    ## 1563        Jiangxi Mainland China 27.61400 115.7221 2020-02-17    35 recovered
    ## 1564          Jilin Mainland China 43.66610 126.1923 2020-02-17     4 recovered
    ## 1565       Liaoning Mainland China 41.29560 122.6085 2020-02-17     3 recovered
    ## 1566        Ningxia Mainland China 37.26920 106.1655 2020-02-17     2 recovered
    ## 1567        Shaanxi Mainland China 35.19170 108.8701 2020-02-17     8 recovered
    ## 1568       Shandong Mainland China 36.34270 118.1498 2020-02-17    18 recovered
    ## 1569       Shanghai Mainland China 31.20200 121.4491 2020-02-17    21 recovered
    ## 1570         Shanxi Mainland China 37.57770 112.2922 2020-02-17     3 recovered
    ## 1571        Sichuan Mainland China 30.61710 102.7103 2020-02-17    25 recovered
    ## 1573        Tianjin Mainland China 39.30540 117.3230 2020-02-17     1 recovered
    ## 1574         Yunnan Mainland China 24.97400 101.4870 2020-02-17     5 recovered
    ## 1575       Zhejiang Mainland China 29.18320 120.0934 2020-02-17    51 recovered
    ## 1579          Anhui Mainland China 31.82570 117.2264 2020-02-18     9 confirmed
    ## 1580        Beijing Mainland China 40.18240 116.4142 2020-02-18     6 confirmed
    ## 1581      Chongqing Mainland China 30.05720 107.8740 2020-02-18     2 confirmed
    ## 1583         Fujian Mainland China 26.07890 117.9874 2020-02-18     2 confirmed
    ## 1584      Guangdong Mainland China 23.34170 113.4244 2020-02-18     6 confirmed
    ## 1585        Guangxi Mainland China 23.82980 108.7881 2020-02-18     4 confirmed
    ## 1586          Hebei Mainland China 38.04280 114.5149 2020-02-18     5 confirmed
    ## 1587   Heilongjiang Mainland China 47.86200 127.7615 2020-02-18     7 confirmed
    ## 1588          Henan Mainland China 33.88202 113.6140 2020-02-18    11 confirmed
    ## 1590          Hubei Mainland China 30.97560 112.2707 2020-02-18  1693 confirmed
    ## 1591          Hunan Mainland China 27.61040 111.7088 2020-02-18     1 confirmed
    ## 1592 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-18     1 confirmed
    ## 1593        Jiangsu Mainland China 32.97110 119.4550 2020-02-18     3 confirmed
    ## 1594        Jiangxi Mainland China 27.61400 115.7221 2020-02-18     3 confirmed
    ## 1595       Shandong Mainland China 36.34270 118.1498 2020-02-18     2 confirmed
    ## 1596         Shanxi Mainland China 37.57770 112.2922 2020-02-18     1 confirmed
    ## 1597        Sichuan Mainland China 30.61710 102.7103 2020-02-18    13 confirmed
    ## 1598        Tianjin Mainland China 39.30540 117.3230 2020-02-18     3 confirmed
    ## 1599       Xinjiang Mainland China 41.11290  85.2401 2020-02-18     1 confirmed
    ## 1600         Yunnan Mainland China 24.97400 101.4870 2020-02-18     1 confirmed
    ## 1601       Zhejiang Mainland China 29.18320 120.0934 2020-02-18     1 confirmed
    ## 1602        Guizhou Mainland China 26.81540 106.8748 2020-02-18     1     death
    ## 1603          Hebei Mainland China 38.04280 114.5149 2020-02-18     1     death
    ## 1604          Henan Mainland China 33.88202 113.6140 2020-02-18     3     death
    ## 1605          Hubei Mainland China 30.97560 112.2707 2020-02-18   132     death
    ## 1606          Hunan Mainland China 27.61040 111.7088 2020-02-18     1     death
    ## 1607       Shandong Mainland China 36.34270 118.1498 2020-02-18     1     death
    ## 1613          Anhui Mainland China 31.82570 117.2264 2020-02-18    81 recovered
    ## 1614        Beijing Mainland China 40.18240 116.4142 2020-02-18     8 recovered
    ## 1615      Chongqing Mainland China 30.05720 107.8740 2020-02-18    29 recovered
    ## 1616         Fujian Mainland China 26.07890 117.9874 2020-02-18     3 recovered
    ## 1617          Gansu Mainland China 36.06110 103.8343 2020-02-18     4 recovered
    ## 1618      Guangdong Mainland China 23.34170 113.4244 2020-02-18    41 recovered
    ## 1619        Guangxi Mainland China 23.82980 108.7881 2020-02-18    16 recovered
    ## 1620        Guizhou Mainland China 26.81540 106.8748 2020-02-18     9 recovered
    ## 1621         Hainan Mainland China 19.19590 109.7453 2020-02-18    20 recovered
    ## 1622          Hebei Mainland China 38.04280 114.5149 2020-02-18    14 recovered
    ## 1623   Heilongjiang Mainland China 47.86200 127.7615 2020-02-18    26 recovered
    ## 1624          Henan Mainland China 33.88202 113.6140 2020-02-18    13 recovered
    ## 1625          Hubei Mainland China 30.97560 112.2707 2020-02-18  1266 recovered
    ## 1626          Hunan Mainland China 27.61040 111.7088 2020-02-18    29 recovered
    ## 1627        Jiangsu Mainland China 32.97110 119.4550 2020-02-18    22 recovered
    ## 1628        Jiangxi Mainland China 27.61400 115.7221 2020-02-18    35 recovered
    ## 1629          Jilin Mainland China 43.66610 126.1923 2020-02-18     2 recovered
    ## 1630       Liaoning Mainland China 41.29560 122.6085 2020-02-18    10 recovered
    ## 1631        Ningxia Mainland China 37.26920 106.1655 2020-02-18     7 recovered
    ## 1632        Qinghai Mainland China 35.74520  95.9956 2020-02-18     2 recovered
    ## 1633        Shaanxi Mainland China 35.19170 108.8701 2020-02-18    10 recovered
    ## 1634       Shandong Mainland China 36.34270 118.1498 2020-02-18    20 recovered
    ## 1635       Shanghai Mainland China 31.20200 121.4491 2020-02-18    16 recovered
    ## 1636         Shanxi Mainland China 37.57770 112.2922 2020-02-18     8 recovered
    ## 1637        Sichuan Mainland China 30.61710 102.7103 2020-02-18    13 recovered
    ## 1638        Tianjin Mainland China 39.30540 117.3230 2020-02-18     2 recovered
    ## 1639         Yunnan Mainland China 24.97400 101.4870 2020-02-18    10 recovered
    ## 1640       Zhejiang Mainland China 29.18320 120.0934 2020-02-18    28 recovered
    ## 1644          Anhui Mainland China 31.82570 117.2264 2020-02-19     4 confirmed
    ## 1645        Beijing Mainland China 40.18240 116.4142 2020-02-19     6 confirmed
    ## 1646      Chongqing Mainland China 30.05720 107.8740 2020-02-19     5 confirmed
    ## 1648         Fujian Mainland China 26.07890 117.9874 2020-02-19     1 confirmed
    ## 1649      Guangdong Mainland China 23.34170 113.4244 2020-02-19     3 confirmed
    ## 1650        Guangxi Mainland China 23.82980 108.7881 2020-02-19     2 confirmed
    ## 1651         Hainan Mainland China 19.19590 109.7453 2020-02-19     5 confirmed
    ## 1652   Heilongjiang Mainland China 47.86200 127.7615 2020-02-19     6 confirmed
    ## 1653          Henan Mainland China 33.88202 113.6140 2020-02-19     5 confirmed
    ## 1655          Hubei Mainland China 30.97560 112.2707 2020-02-19   349 confirmed
    ## 1656          Hunan Mainland China 27.61040 111.7088 2020-02-19     1 confirmed
    ## 1657 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-19     2 confirmed
    ## 1658        Jiangsu Mainland China 32.97110 119.4550 2020-02-19     2 confirmed
    ## 1659        Jiangxi Mainland China 27.61400 115.7221 2020-02-19     1 confirmed
    ## 1660          Jilin Mainland China 43.66610 126.1923 2020-02-19     1 confirmed
    ## 1661        Ningxia Mainland China 37.26920 106.1655 2020-02-19     1 confirmed
    ## 1662        Shaanxi Mainland China 35.19170 108.8701 2020-02-19     2 confirmed
    ## 1663       Shandong Mainland China 36.34270 118.1498 2020-02-19     1 confirmed
    ## 1664        Sichuan Mainland China 30.61710 102.7103 2020-02-19     6 confirmed
    ## 1666        Tianjin Mainland China 39.30540 117.3230 2020-02-19     2 confirmed
    ## 1667       Zhejiang Mainland China 29.18320 120.0934 2020-02-19     2 confirmed
    ## 1669      Guangdong Mainland China 23.34170 113.4244 2020-02-19     1     death
    ## 1670   Heilongjiang Mainland China 47.86200 127.7615 2020-02-19     1     death
    ## 1672          Hubei Mainland China 30.97560 112.2707 2020-02-19   108     death
    ## 1673       Shanghai Mainland China 31.20200 121.4491 2020-02-19     1     death
    ## 1674         Yunnan Mainland China 24.97400 101.4870 2020-02-19     1     death
    ## 1678          Anhui Mainland China 31.82570 117.2264 2020-02-19    52 recovered
    ## 1679        Beijing Mainland China 40.18240 116.4142 2020-02-19    23 recovered
    ## 1680      Chongqing Mainland China 30.05720 107.8740 2020-02-19    20 recovered
    ## 1682         Fujian Mainland China 26.07890 117.9874 2020-02-19    19 recovered
    ## 1683          Gansu Mainland China 36.06110 103.8343 2020-02-19     3 recovered
    ## 1684      Guangdong Mainland China 23.34170 113.4244 2020-02-19    41 recovered
    ## 1685        Guangxi Mainland China 23.82980 108.7881 2020-02-19    17 recovered
    ## 1686        Guizhou Mainland China 26.81540 106.8748 2020-02-19     4 recovered
    ## 1687         Hainan Mainland China 19.19590 109.7453 2020-02-19     5 recovered
    ## 1688          Hebei Mainland China 38.04280 114.5149 2020-02-19    16 recovered
    ## 1689   Heilongjiang Mainland China 47.86200 127.7615 2020-02-19     9 recovered
    ## 1690          Henan Mainland China 33.88202 113.6140 2020-02-19    51 recovered
    ## 1692          Hubei Mainland China 30.97560 112.2707 2020-02-19  1209 recovered
    ## 1693          Hunan Mainland China 27.61040 111.7088 2020-02-19    34 recovered
    ## 1694 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-19     2 recovered
    ## 1695        Jiangsu Mainland China 32.97110 119.4550 2020-02-19    38 recovered
    ## 1696        Jiangxi Mainland China 27.61400 115.7221 2020-02-19    52 recovered
    ## 1697          Jilin Mainland China 43.66610 126.1923 2020-02-19     1 recovered
    ## 1698       Liaoning Mainland China 41.29560 122.6085 2020-02-19     2 recovered
    ## 1699        Qinghai Mainland China 35.74520  95.9956 2020-02-19     1 recovered
    ## 1700        Shaanxi Mainland China 35.19170 108.8701 2020-02-19    13 recovered
    ## 1701       Shandong Mainland China 36.34270 118.1498 2020-02-19    20 recovered
    ## 1702       Shanghai Mainland China 31.20200 121.4491 2020-02-19     9 recovered
    ## 1703         Shanxi Mainland China 37.57770 112.2922 2020-02-19     7 recovered
    ## 1704        Sichuan Mainland China 30.61710 102.7103 2020-02-19    19 recovered
    ## 1705        Tianjin Mainland China 39.30540 117.3230 2020-02-19     6 recovered
    ## 1706       Xinjiang Mainland China 41.11290  85.2401 2020-02-19     8 recovered
    ## 1707         Yunnan Mainland China 24.97400 101.4870 2020-02-19     3 recovered
    ## 1708       Zhejiang Mainland China 29.18320 120.0934 2020-02-19    69 recovered
    ## 1712          Anhui Mainland China 31.82570 117.2264 2020-02-20     1 confirmed
    ## 1713        Beijing Mainland China 40.18240 116.4142 2020-02-20     2 confirmed
    ## 1714      Chongqing Mainland China 30.05720 107.8740 2020-02-20     7 confirmed
    ## 1716      Guangdong Mainland China 23.34170 113.4244 2020-02-20     1 confirmed
    ## 1717        Guangxi Mainland China 23.82980 108.7881 2020-02-20     1 confirmed
    ## 1718          Hebei Mainland China 38.04280 114.5149 2020-02-20     1 confirmed
    ## 1719   Heilongjiang Mainland China 47.86200 127.7615 2020-02-20     6 confirmed
    ## 1720          Henan Mainland China 33.88202 113.6140 2020-02-20     3 confirmed
    ## 1722          Hubei Mainland China 30.97560 112.2707 2020-02-20   411 confirmed
    ## 1723          Hunan Mainland China 27.61040 111.7088 2020-02-20     2 confirmed
    ## 1724          Jilin Mainland China 43.66610 126.1923 2020-02-20     1 confirmed
    ## 1725        Shaanxi Mainland China 35.19170 108.8701 2020-02-20     3 confirmed
    ## 1726       Shandong Mainland China 36.34270 118.1498 2020-02-20     2 confirmed
    ## 1727       Shanghai Mainland China 31.20200 121.4491 2020-02-20     1 confirmed
    ## 1728         Shanxi Mainland China 37.57770 112.2922 2020-02-20     1 confirmed
    ## 1729        Sichuan Mainland China 30.61710 102.7103 2020-02-20     6 confirmed
    ## 1731        Tianjin Mainland China 39.30540 117.3230 2020-02-20     1 confirmed
    ## 1732         Yunnan Mainland China 24.97400 101.4870 2020-02-20     2 confirmed
    ## 1733       Zhejiang Mainland China 29.18320 120.0934 2020-02-20     1 confirmed
    ## 1735      Chongqing Mainland China 30.05720 107.8740 2020-02-20     1     death
    ## 1737         Fujian Mainland China 26.07890 117.9874 2020-02-20     1     death
    ## 1738          Hebei Mainland China 38.04280 114.5149 2020-02-20     1     death
    ## 1739          Hubei Mainland China 30.97560 112.2707 2020-02-20   115     death
    ## 1740        Shaanxi Mainland China 35.19170 108.8701 2020-02-20     1     death
    ## 1741       Shandong Mainland China 36.34270 118.1498 2020-02-20     1     death
    ## 1742         Yunnan Mainland China 24.97400 101.4870 2020-02-20     1     death
    ## 1743       Zhejiang Mainland China 29.18320 120.0934 2020-02-20     1     death
    ## 1745          Anhui Mainland China 31.82570 117.2264 2020-02-20    61 recovered
    ## 1746        Beijing Mainland China 40.18240 116.4142 2020-02-20     8 recovered
    ## 1747      Chongqing Mainland China 30.05720 107.8740 2020-02-20    25 recovered
    ## 1748         Fujian Mainland China 26.07890 117.9874 2020-02-20    14 recovered
    ## 1749          Gansu Mainland China 36.06110 103.8343 2020-02-20     6 recovered
    ## 1750      Guangdong Mainland China 23.34170 113.4244 2020-02-20    36 recovered
    ## 1751        Guangxi Mainland China 23.82980 108.7881 2020-02-20     4 recovered
    ## 1752        Guizhou Mainland China 26.81540 106.8748 2020-02-20     2 recovered
    ## 1753         Hainan Mainland China 19.19590 109.7453 2020-02-20     2 recovered
    ## 1754          Hebei Mainland China 38.04280 114.5149 2020-02-20    17 recovered
    ## 1755   Heilongjiang Mainland China 47.86200 127.7615 2020-02-20    16 recovered
    ## 1756          Henan Mainland China 33.88202 113.6140 2020-02-20    64 recovered
    ## 1758          Hubei Mainland China 30.97560 112.2707 2020-02-20  1451 recovered
    ## 1759          Hunan Mainland China 27.61040 111.7088 2020-02-20    73 recovered
    ## 1760 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-20     6 recovered
    ## 1761        Jiangsu Mainland China 32.97110 119.4550 2020-02-20    38 recovered
    ## 1762        Jiangxi Mainland China 27.61400 115.7221 2020-02-20    71 recovered
    ## 1763          Jilin Mainland China 43.66610 126.1923 2020-02-20     6 recovered
    ## 1764       Liaoning Mainland China 41.29560 122.6085 2020-02-20     4 recovered
    ## 1766        Ningxia Mainland China 37.26920 106.1655 2020-02-20     2 recovered
    ## 1767        Shaanxi Mainland China 35.19170 108.8701 2020-02-20    16 recovered
    ## 1768       Shandong Mainland China 36.34270 118.1498 2020-02-20    23 recovered
    ## 1769       Shanghai Mainland China 31.20200 121.4491 2020-02-20    13 recovered
    ## 1770         Shanxi Mainland China 37.57770 112.2922 2020-02-20     8 recovered
    ## 1771        Sichuan Mainland China 30.61710 102.7103 2020-02-20    29 recovered
    ## 1772        Tianjin Mainland China 39.30540 117.3230 2020-02-20     5 recovered
    ## 1773       Xinjiang Mainland China 41.11290  85.2401 2020-02-20     2 recovered
    ## 1774         Yunnan Mainland China 24.97400 101.4870 2020-02-20    19 recovered
    ## 1775       Zhejiang Mainland China 29.18320 120.0934 2020-02-20    29 recovered
    ## 1783          Anhui Mainland China 31.82570 117.2264 2020-02-21     1 confirmed
    ## 1784        Beijing Mainland China 40.18240 116.4142 2020-02-21     1 confirmed
    ## 1786      Chongqing Mainland China 30.05720 107.8740 2020-02-21     5 confirmed
    ## 1788      Guangdong Mainland China 23.34170 113.4244 2020-02-21     1 confirmed
    ## 1789        Guangxi Mainland China 23.82980 108.7881 2020-02-21     1 confirmed
    ## 1790          Hebei Mainland China 38.04280 114.5149 2020-02-21     1 confirmed
    ## 1791   Heilongjiang Mainland China 47.86200 127.7615 2020-02-21     3 confirmed
    ## 1792          Henan Mainland China 33.88202 113.6140 2020-02-21     2 confirmed
    ## 1793          Hubei Mainland China 30.97560 112.2707 2020-02-21   220 confirmed
    ## 1795          Hunan Mainland China 27.61040 111.7088 2020-02-21     1 confirmed
    ## 1799       Shandong Mainland China 36.34270 118.1498 2020-02-21   203 confirmed
    ## 1800        Sichuan Mainland China 30.61710 102.7103 2020-02-21     5 confirmed
    ## 1802        Tianjin Mainland China 39.30540 117.3230 2020-02-21     1 confirmed
    ## 1804       Zhejiang Mainland China 29.18320 120.0934 2020-02-21    28 confirmed
    ## 1813          Anhui Mainland China 31.82570 117.2264 2020-02-21    65 recovered
    ## 1814        Beijing Mainland China 40.18240 116.4142 2020-02-21    16 recovered
    ## 1815      Chongqing Mainland China 30.05720 107.8740 2020-02-21    17 recovered
    ## 1816         Fujian Mainland China 26.07890 117.9874 2020-02-21    23 recovered
    ## 1817          Gansu Mainland China 36.06110 103.8343 2020-02-21     5 recovered
    ## 1818      Guangdong Mainland China 23.34170 113.4244 2020-02-21    48 recovered
    ## 1819        Guangxi Mainland China 23.82980 108.7881 2020-02-21     7 recovered
    ## 1820        Guizhou Mainland China 26.81540 106.8748 2020-02-21     5 recovered
    ## 1821         Hainan Mainland China 19.19590 109.7453 2020-02-21     9 recovered
    ## 1822          Hebei Mainland China 38.04280 114.5149 2020-02-21    15 recovered
    ## 1823   Heilongjiang Mainland China 47.86200 127.7615 2020-02-21    39 recovered
    ## 1824          Henan Mainland China 33.88202 113.6140 2020-02-21    99 recovered
    ## 1826          Hubei Mainland China 30.97560 112.2707 2020-02-21    93 recovered
    ## 1827          Hunan Mainland China 27.61040 111.7088 2020-02-21    27 recovered
    ## 1828 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-21     1 recovered
    ## 1829        Jiangsu Mainland China 32.97110 119.4550 2020-02-21    17 recovered
    ## 1830        Jiangxi Mainland China 27.61400 115.7221 2020-02-21    56 recovered
    ## 1831          Jilin Mainland China 43.66610 126.1923 2020-02-21     2 recovered
    ## 1832       Liaoning Mainland China 41.29560 122.6085 2020-02-21     2 recovered
    ## 1833        Ningxia Mainland China 37.26920 106.1655 2020-02-21     4 recovered
    ## 1834        Qinghai Mainland China 35.74520  95.9956 2020-02-21     2 recovered
    ## 1838        Shaanxi Mainland China 35.19170 108.8701 2020-02-21    16 recovered
    ## 1839       Shandong Mainland China 36.34270 118.1498 2020-02-21    27 recovered
    ## 1840       Shanghai Mainland China 31.20200 121.4491 2020-02-21    12 recovered
    ## 1841         Shanxi Mainland China 37.57770 112.2922 2020-02-21     2 recovered
    ## 1842        Sichuan Mainland China 30.61710 102.7103 2020-02-21    14 recovered
    ## 1843        Tianjin Mainland China 39.30540 117.3230 2020-02-21     3 recovered
    ## 1845       Xinjiang Mainland China 41.11290  85.2401 2020-02-21     2 recovered
    ## 1846         Yunnan Mainland China 24.97400 101.4870 2020-02-21    17 recovered
    ## 1847       Zhejiang Mainland China 29.18320 120.0934 2020-02-21    46 recovered
    ## 1853          Anhui Mainland China 31.82570 117.2264 2020-02-22     1 confirmed
    ## 1854        Beijing Mainland China 40.18240 116.4142 2020-02-22     3 confirmed
    ## 1855      Chongqing Mainland China 30.05720 107.8740 2020-02-22     1 confirmed
    ## 1857      Guangdong Mainland China 23.34170 113.4244 2020-02-22     6 confirmed
    ## 1858        Guangxi Mainland China 23.82980 108.7881 2020-02-22     3 confirmed
    ## 1859          Hebei Mainland China 38.04280 114.5149 2020-02-22     1 confirmed
    ## 1860          Henan Mainland China 33.88202 113.6140 2020-02-22     3 confirmed
    ## 1862          Hubei Mainland China 30.97560 112.2707 2020-02-22  1422 confirmed
    ## 1863          Hunan Mainland China 27.61040 111.7088 2020-02-22     2 confirmed
    ## 1864       Shandong Mainland China 36.34270 118.1498 2020-02-22     1 confirmed
    ## 1865       Shanghai Mainland China 31.20200 121.4491 2020-02-22     1 confirmed
    ## 1866        Sichuan Mainland China 30.61710 102.7103 2020-02-22     1 confirmed
    ## 1867        Tianjin Mainland China 39.30540 117.3230 2020-02-22     3 confirmed
    ## 1868       Zhejiang Mainland China 29.18320 120.0934 2020-02-22     2 confirmed
    ## 1871          Hebei Mainland China 38.04280 114.5149 2020-02-22     1     death
    ## 1872          Hubei Mainland China 30.97560 112.2707 2020-02-22   202     death
    ## 1873       Shanghai Mainland China 31.20200 121.4491 2020-02-22     1     death
    ## 1874       Xinjiang Mainland China 41.11290  85.2401 2020-02-22     1     death
    ## 1876          Anhui Mainland China 31.82570 117.2264 2020-02-22    58 recovered
    ## 1877        Beijing Mainland China 40.18240 116.4142 2020-02-22     9 recovered
    ## 1878      Chongqing Mainland China 30.05720 107.8740 2020-02-22    12 recovered
    ## 1879         Fujian Mainland China 26.07890 117.9874 2020-02-22    13 recovered
    ## 1880      Guangdong Mainland China 23.34170 113.4244 2020-02-22    38 recovered
    ## 1881        Guangxi Mainland China 23.82980 108.7881 2020-02-22     7 recovered
    ## 1882        Guizhou Mainland China 26.81540 106.8748 2020-02-22    13 recovered
    ## 1883         Hainan Mainland China 19.19590 109.7453 2020-02-22     9 recovered
    ## 1884          Hebei Mainland China 38.04280 114.5149 2020-02-22    19 recovered
    ## 1885   Heilongjiang Mainland China 47.86200 127.7615 2020-02-22    29 recovered
    ## 1886          Henan Mainland China 33.88202 113.6140 2020-02-22    94 recovered
    ## 1888          Hubei Mainland China 30.97560 112.2707 2020-02-22  3418 recovered
    ## 1889          Hunan Mainland China 27.61040 111.7088 2020-02-22    31 recovered
    ## 1890 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-22     9 recovered
    ## 1891        Jiangsu Mainland China 32.97110 119.4550 2020-02-22    28 recovered
    ## 1892        Jiangxi Mainland China 27.61400 115.7221 2020-02-22    66 recovered
    ## 1893          Jilin Mainland China 43.66610 126.1923 2020-02-22     7 recovered
    ## 1894       Liaoning Mainland China 41.29560 122.6085 2020-02-22     5 recovered
    ## 1895        Shaanxi Mainland China 35.19170 108.8701 2020-02-22    15 recovered
    ## 1896       Shandong Mainland China 36.34270 118.1498 2020-02-22    21 recovered
    ## 1897       Shanghai Mainland China 31.20200 121.4491 2020-02-22    16 recovered
    ## 1898         Shanxi Mainland China 37.57770 112.2922 2020-02-22     3 recovered
    ## 1899        Sichuan Mainland China 30.61710 102.7103 2020-02-22    19 recovered
    ## 1900        Tianjin Mainland China 39.30540 117.3230 2020-02-22     3 recovered
    ## 1901       Xinjiang Mainland China 41.11290  85.2401 2020-02-22     1 recovered
    ## 1902         Yunnan Mainland China 24.97400 101.4870 2020-02-22    11 recovered
    ## 1903       Zhejiang Mainland China 29.18320 120.0934 2020-02-22    40 recovered
    ## 1909      Chongqing Mainland China 30.05720 107.8740 2020-02-23     2 confirmed
    ## 1911      Guangdong Mainland China 23.34170 113.4244 2020-02-23     3 confirmed
    ## 1912          Hebei Mainland China 38.04280 114.5149 2020-02-23     2 confirmed
    ## 1913   Heilongjiang Mainland China 47.86200 127.7615 2020-02-23     1 confirmed
    ## 1914          Henan Mainland China 33.88202 113.6140 2020-02-23     1 confirmed
    ## 1916          Hunan Mainland China 27.61040 111.7088 2020-02-23     3 confirmed
    ## 1917       Shandong Mainland China 36.34270 118.1498 2020-02-23     4 confirmed
    ## 1923      Guangdong Mainland China 23.34170 113.4244 2020-02-23     1     death
    ## 1924         Hainan Mainland China 19.19590 109.7453 2020-02-23     1     death
    ## 1929          Anhui Mainland China 31.82570 117.2264 2020-02-23    40 recovered
    ## 1930        Beijing Mainland China 40.18240 116.4142 2020-02-23    11 recovered
    ## 1931      Chongqing Mainland China 30.05720 107.8740 2020-02-23     7 recovered
    ## 1933         Fujian Mainland China 26.07890 117.9874 2020-02-23     8 recovered
    ## 1934          Gansu Mainland China 36.06110 103.8343 2020-02-23     2 recovered
    ## 1935      Guangdong Mainland China 23.34170 113.4244 2020-02-23    27 recovered
    ## 1936        Guangxi Mainland China 23.82980 108.7881 2020-02-23     2 recovered
    ## 1937        Guizhou Mainland China 26.81540 106.8748 2020-02-23    12 recovered
    ## 1938         Hainan Mainland China 19.19590 109.7453 2020-02-23     2 recovered
    ## 1939          Hebei Mainland China 38.04280 114.5149 2020-02-23    16 recovered
    ## 1940   Heilongjiang Mainland China 47.86200 127.7615 2020-02-23    18 recovered
    ## 1941          Henan Mainland China 33.88202 113.6140 2020-02-23    38 recovered
    ## 1943          Hubei Mainland China 30.97560 112.2707 2020-02-23    44 recovered
    ## 1944          Hunan Mainland China 27.61040 111.7088 2020-02-23    22 recovered
    ## 1945 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-23     1 recovered
    ## 1946        Jiangsu Mainland China 32.97110 119.4550 2020-02-23    17 recovered
    ## 1947        Jiangxi Mainland China 27.61400 115.7221 2020-02-23    58 recovered
    ## 1948          Jilin Mainland China 43.66610 126.1923 2020-02-23     2 recovered
    ## 1949       Liaoning Mainland China 41.29560 122.6085 2020-02-23     7 recovered
    ## 1950        Ningxia Mainland China 37.26920 106.1655 2020-02-23     8 recovered
    ## 1951        Shaanxi Mainland China 35.19170 108.8701 2020-02-23    14 recovered
    ## 1952       Shandong Mainland China 36.34270 118.1498 2020-02-23    19 recovered
    ## 1953       Shanghai Mainland China 31.20200 121.4491 2020-02-23    22 recovered
    ## 1954         Shanxi Mainland China 37.57770 112.2922 2020-02-23     7 recovered
    ## 1955        Sichuan Mainland China 30.61710 102.7103 2020-02-23    11 recovered
    ## 1956        Tianjin Mainland China 39.30540 117.3230 2020-02-23    16 recovered
    ## 1957       Xinjiang Mainland China 41.11290  85.2401 2020-02-23     3 recovered
    ## 1958         Yunnan Mainland China 24.97400 101.4870 2020-02-23     8 recovered
    ## 1959       Zhejiang Mainland China 29.18320 120.0934 2020-02-23    41 recovered
    ## 1970      Chongqing Mainland China 30.05720 107.8740 2020-02-24     1 confirmed
    ## 1971      Guangdong Mainland China 23.34170 113.4244 2020-02-24     3 confirmed
    ## 1972        Guangxi Mainland China 23.82980 108.7881 2020-02-24     2 confirmed
    ## 1974          Hubei Mainland China 30.97560 112.2707 2020-02-24   203 confirmed
    ## 1975          Jilin Mainland China 43.66610 126.1923 2020-02-24     2 confirmed
    ## 1978       Shandong Mainland China 36.34270 118.1498 2020-02-24     1 confirmed
    ## 1979         Shanxi Mainland China 37.57770 112.2922 2020-02-24     1 confirmed
    ## 1980        Sichuan Mainland China 30.61710 102.7103 2020-02-24     1 confirmed
    ## 1988          Hubei Mainland China 30.97560 112.2707 2020-02-24   149     death
    ## 1989       Shandong Mainland China 36.34270 118.1498 2020-02-24     1     death
    ## 1992          Anhui Mainland China 31.82570 117.2264 2020-02-24    26 recovered
    ## 1993        Beijing Mainland China 40.18240 116.4142 2020-02-24     9 recovered
    ## 1994      Chongqing Mainland China 30.05720 107.8740 2020-02-24    14 recovered
    ## 1995         Fujian Mainland China 26.07890 117.9874 2020-02-24    13 recovered
    ## 1996          Gansu Mainland China 36.06110 103.8343 2020-02-24     2 recovered
    ## 1997      Guangdong Mainland China 23.34170 113.4244 2020-02-24    31 recovered
    ## 1998        Guangxi Mainland China 23.82980 108.7881 2020-02-24     6 recovered
    ## 1999         Hainan Mainland China 19.19590 109.7453 2020-02-24    10 recovered
    ## 2000          Hebei Mainland China 38.04280 114.5149 2020-02-24    15 recovered
    ## 2001   Heilongjiang Mainland China 47.86200 127.7615 2020-02-24     5 recovered
    ## 2002          Henan Mainland China 33.88202 113.6140 2020-02-24    75 recovered
    ## 2004          Hubei Mainland China 30.97560 112.2707 2020-02-24  1405 recovered
    ## 2005          Hunan Mainland China 27.61040 111.7088 2020-02-24    17 recovered
    ## 2006 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-24     7 recovered
    ## 2007        Jiangsu Mainland China 32.97110 119.4550 2020-02-24    34 recovered
    ## 2008        Jiangxi Mainland China 27.61400 115.7221 2020-02-24    32 recovered
    ## 2009          Jilin Mainland China 43.66610 126.1923 2020-02-24     6 recovered
    ## 2010       Liaoning Mainland China 41.29560 122.6085 2020-02-24     7 recovered
    ## 2011        Ningxia Mainland China 37.26920 106.1655 2020-02-24     2 recovered
    ## 2012        Shaanxi Mainland China 35.19170 108.8701 2020-02-24    10 recovered
    ## 2013       Shandong Mainland China 36.34270 118.1498 2020-02-24    22 recovered
    ## 2014       Shanghai Mainland China 31.20200 121.4491 2020-02-24    12 recovered
    ## 2015         Shanxi Mainland China 37.57770 112.2922 2020-02-24     6 recovered
    ## 2016        Sichuan Mainland China 30.61710 102.7103 2020-02-24    15 recovered
    ## 2018        Tianjin Mainland China 39.30540 117.3230 2020-02-24     6 recovered
    ## 2019       Xinjiang Mainland China 41.11290  85.2401 2020-02-24     2 recovered
    ## 2020         Yunnan Mainland China 24.97400 101.4870 2020-02-24     9 recovered
    ## 2021       Zhejiang Mainland China 29.18320 120.0934 2020-02-24    22 recovered
    ## 2037        Beijing Mainland China 40.18240 116.4142 2020-02-25     1 confirmed
    ## 2039         Fujian Mainland China 26.07890 117.9874 2020-02-25     1 confirmed
    ## 2040      Guangdong Mainland China 23.34170 113.4244 2020-02-25     2 confirmed
    ## 2041        Guangxi Mainland China 23.82980 108.7881 2020-02-25     1 confirmed
    ## 2043          Hubei Mainland China 30.97560 112.2707 2020-02-25   499 confirmed
    ## 2044       Shandong Mainland China 36.34270 118.1498 2020-02-25     1 confirmed
    ## 2045       Shanghai Mainland China 31.20200 121.4491 2020-02-25     1 confirmed
    ## 2046        Sichuan Mainland China 30.61710 102.7103 2020-02-25     2 confirmed
    ## 2051      Guangdong Mainland China 23.34170 113.4244 2020-02-25     1     death
    ## 2052          Hubei Mainland China 30.97560 112.2707 2020-02-25    68     death
    ## 2053       Shandong Mainland China 36.34270 118.1498 2020-02-25     1     death
    ## 2059          Anhui Mainland China 31.82570 117.2264 2020-02-25    49 recovered
    ## 2060        Beijing Mainland China 40.18240 116.4142 2020-02-25    17 recovered
    ## 2061      Chongqing Mainland China 30.05720 107.8740 2020-02-25    23 recovered
    ## 2062         Fujian Mainland China 26.07890 117.9874 2020-02-25    16 recovered
    ## 2063      Guangdong Mainland China 23.34170 113.4244 2020-02-25    36 recovered
    ## 2064        Guangxi Mainland China 23.82980 108.7881 2020-02-25    22 recovered
    ## 2065        Guizhou Mainland China 26.81540 106.8748 2020-02-25     2 recovered
    ## 2066         Hainan Mainland China 19.19590 109.7453 2020-02-25     8 recovered
    ## 2067          Hebei Mainland China 38.04280 114.5149 2020-02-25    14 recovered
    ## 2068   Heilongjiang Mainland China 47.86200 127.7615 2020-02-25    16 recovered
    ## 2069          Henan Mainland China 33.88202 113.6140 2020-02-25    59 recovered
    ## 2070          Hubei Mainland China 30.97560 112.2707 2020-02-25  2223 recovered
    ## 2071          Hunan Mainland China 27.61040 111.7088 2020-02-25    37 recovered
    ## 2072 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-25     1 recovered
    ## 2073        Jiangsu Mainland China 32.97110 119.4550 2020-02-25     6 recovered
    ## 2074        Jiangxi Mainland China 27.61400 115.7221 2020-02-25    38 recovered
    ## 2075          Jilin Mainland China 43.66610 126.1923 2020-02-25     3 recovered
    ## 2076       Liaoning Mainland China 41.29560 122.6085 2020-02-25     3 recovered
    ## 2078        Ningxia Mainland China 37.26920 106.1655 2020-02-25     3 recovered
    ## 2079        Shaanxi Mainland China 35.19170 108.8701 2020-02-25    13 recovered
    ## 2080       Shandong Mainland China 36.34270 118.1498 2020-02-25    12 recovered
    ## 2081       Shanghai Mainland China 31.20200 121.4491 2020-02-25     7 recovered
    ## 2082         Shanxi Mainland China 37.57770 112.2922 2020-02-25     4 recovered
    ## 2083        Sichuan Mainland China 30.61710 102.7103 2020-02-25    13 recovered
    ## 2085        Tianjin Mainland China 39.30540 117.3230 2020-02-25     4 recovered
    ## 2086         Yunnan Mainland China 24.97400 101.4870 2020-02-25     5 recovered
    ## 2087       Zhejiang Mainland China 29.18320 120.0934 2020-02-25    26 recovered
    ## 2114          Hebei Mainland China 38.04280 114.5149 2020-02-26     1 confirmed
    ## 2116          Hubei Mainland China 30.97560 112.2707 2020-02-26   401 confirmed
    ## 2117       Shanghai Mainland China 31.20200 121.4491 2020-02-26     1 confirmed
    ## 2118        Sichuan Mainland China 30.61710 102.7103 2020-02-26     2 confirmed
    ## 2127          Hubei Mainland China 30.97560 112.2707 2020-02-26    52     death
    ## 2132          Anhui Mainland China 31.82570 117.2264 2020-02-26    32 recovered
    ## 2133        Beijing Mainland China 40.18240 116.4142 2020-02-26    20 recovered
    ## 2134      Chongqing Mainland China 30.05720 107.8740 2020-02-26    12 recovered
    ## 2136         Fujian Mainland China 26.07890 117.9874 2020-02-26    19 recovered
    ## 2137          Gansu Mainland China 36.06110 103.8343 2020-02-26     1 recovered
    ## 2138      Guangdong Mainland China 23.34170 113.4244 2020-02-26    29 recovered
    ## 2139        Guangxi Mainland China 23.82980 108.7881 2020-02-26    13 recovered
    ## 2140         Hainan Mainland China 19.19590 109.7453 2020-02-26     5 recovered
    ## 2141          Hebei Mainland China 38.04280 114.5149 2020-02-26    13 recovered
    ## 2142   Heilongjiang Mainland China 47.86200 127.7615 2020-02-26     6 recovered
    ## 2143          Henan Mainland China 33.88202 113.6140 2020-02-26    31 recovered
    ## 2145          Hubei Mainland China 30.97560 112.2707 2020-02-26  1998 recovered
    ## 2146          Hunan Mainland China 27.61040 111.7088 2020-02-26    15 recovered
    ## 2147 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-26     3 recovered
    ## 2148        Jiangsu Mainland China 32.97110 119.4550 2020-02-26    20 recovered
    ## 2149        Jiangxi Mainland China 27.61400 115.7221 2020-02-26    36 recovered
    ## 2150          Jilin Mainland China 43.66610 126.1923 2020-02-26     2 recovered
    ## 2151       Liaoning Mainland China 41.29560 122.6085 2020-02-26     5 recovered
    ## 2152        Ningxia Mainland China 37.26920 106.1655 2020-02-26     4 recovered
    ## 2153        Shaanxi Mainland China 35.19170 108.8701 2020-02-26     6 recovered
    ## 2154       Shandong Mainland China 36.34270 118.1498 2020-02-26    22 recovered
    ## 2155       Shanghai Mainland China 31.20200 121.4491 2020-02-26     4 recovered
    ## 2156         Shanxi Mainland China 37.57770 112.2922 2020-02-26     6 recovered
    ## 2157        Sichuan Mainland China 30.61710 102.7103 2020-02-26    18 recovered
    ## 2158        Tianjin Mainland China 39.30540 117.3230 2020-02-26     5 recovered
    ## 2159       Xinjiang Mainland China 41.11290  85.2401 2020-02-26     4 recovered
    ## 2160         Yunnan Mainland China 24.97400 101.4870 2020-02-26    15 recovered
    ## 2161       Zhejiang Mainland China 29.18320 120.0934 2020-02-26    59 recovered
    ## 2182        Beijing Mainland China 40.18240 116.4142 2020-02-27    10 confirmed
    ## 2184         Fujian Mainland China 26.07890 117.9874 2020-02-27     2 confirmed
    ## 2185          Hebei Mainland China 38.04280 114.5149 2020-02-27     5 confirmed
    ## 2186          Henan Mainland China 33.88202 113.6140 2020-02-27     1 confirmed
    ## 2188          Hubei Mainland China 30.97560 112.2707 2020-02-27   409 confirmed
    ## 2189          Hunan Mainland China 27.61040 111.7088 2020-02-27     1 confirmed
    ## 2190        Ningxia Mainland China 37.26920 106.1655 2020-02-27     1 confirmed
    ## 2192        Sichuan Mainland China 30.61710 102.7103 2020-02-27     3 confirmed
    ## 2193        Tianjin Mainland China 39.30540 117.3230 2020-02-27     1 confirmed
    ## 2199        Beijing Mainland China 40.18240 116.4142 2020-02-27     1     death
    ## 2200   Heilongjiang Mainland China 47.86200 127.7615 2020-02-27     1     death
    ## 2201          Henan Mainland China 33.88202 113.6140 2020-02-27     1     death
    ## 2202          Hubei Mainland China 30.97560 112.2707 2020-02-27    26     death
    ## 2206          Anhui Mainland China 31.82570 117.2264 2020-02-27    48 recovered
    ## 2207        Beijing Mainland China 40.18240 116.4142 2020-02-27    13 recovered
    ## 2209      Chongqing Mainland China 30.05720 107.8740 2020-02-27    17 recovered
    ## 2210         Fujian Mainland China 26.07890 117.9874 2020-02-27    10 recovered
    ## 2211      Guangdong Mainland China 23.34170 113.4244 2020-02-27    39 recovered
    ## 2212        Guangxi Mainland China 23.82980 108.7881 2020-02-27    14 recovered
    ## 2213        Guizhou Mainland China 26.81540 106.8748 2020-02-27     8 recovered
    ## 2214         Hainan Mainland China 19.19590 109.7453 2020-02-27     2 recovered
    ## 2215          Hebei Mainland China 38.04280 114.5149 2020-02-27    13 recovered
    ## 2216   Heilongjiang Mainland China 47.86200 127.7615 2020-02-27    21 recovered
    ## 2217          Henan Mainland China 33.88202 113.6140 2020-02-27    35 recovered
    ## 2218          Hubei Mainland China 30.97560 112.2707 2020-02-27  2414 recovered
    ## 2219          Hunan Mainland China 27.61040 111.7088 2020-02-27    21 recovered
    ## 2220 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-27     5 recovered
    ## 2221        Jiangsu Mainland China 32.97110 119.4550 2020-02-27    20 recovered
    ## 2222        Jiangxi Mainland China 27.61400 115.7221 2020-02-27    35 recovered
    ## 2223          Jilin Mainland China 43.66610 126.1923 2020-02-27     2 recovered
    ## 2224       Liaoning Mainland China 41.29560 122.6085 2020-02-27     5 recovered
    ## 2226        Ningxia Mainland China 37.26920 106.1655 2020-02-27     3 recovered
    ## 2227        Shaanxi Mainland China 35.19170 108.8701 2020-02-27     3 recovered
    ## 2228       Shandong Mainland China 36.34270 118.1498 2020-02-27    10 recovered
    ## 2229       Shanghai Mainland China 31.20200 121.4491 2020-02-27     4 recovered
    ## 2230         Shanxi Mainland China 37.57770 112.2922 2020-02-27     3 recovered
    ## 2231        Sichuan Mainland China 30.61710 102.7103 2020-02-27    14 recovered
    ## 2232        Tianjin Mainland China 39.30540 117.3230 2020-02-27     6 recovered
    ## 2233       Xinjiang Mainland China 41.11290  85.2401 2020-02-27     9 recovered
    ## 2234         Yunnan Mainland China 24.97400 101.4870 2020-02-27     6 recovered
    ## 2235       Zhejiang Mainland China 29.18320 120.0934 2020-02-27    65 recovered
    ## 2260          Anhui Mainland China 31.82570 117.2264 2020-02-28     1 confirmed
    ## 2261      Guangdong Mainland China 23.34170 113.4244 2020-02-28     1 confirmed
    ## 2262          Hebei Mainland China 38.04280 114.5149 2020-02-28     1 confirmed
    ## 2264          Hubei Mainland China 30.97560 112.2707 2020-02-28   318 confirmed
    ## 2265        Jiangxi Mainland China 27.61400 115.7221 2020-02-28     1 confirmed
    ## 2266        Sichuan Mainland China 30.61710 102.7103 2020-02-28     4 confirmed
    ## 2271        Beijing Mainland China 40.18240 116.4142 2020-02-28     2     death
    ## 2273          Hubei Mainland China 30.97560 112.2707 2020-02-28    41     death
    ## 2274       Xinjiang Mainland China 41.11290  85.2401 2020-02-28     1     death
    ## 2280          Anhui Mainland China 31.82570 117.2264 2020-02-28    29 recovered
    ## 2281        Beijing Mainland China 40.18240 116.4142 2020-02-28     9 recovered
    ## 2283      Chongqing Mainland China 30.05720 107.8740 2020-02-28    21 recovered
    ## 2284         Fujian Mainland China 26.07890 117.9874 2020-02-28     7 recovered
    ## 2285          Gansu Mainland China 36.06110 103.8343 2020-02-28     1 recovered
    ## 2286      Guangdong Mainland China 23.34170 113.4244 2020-02-28    45 recovered
    ## 2287        Guangxi Mainland China 23.82980 108.7881 2020-02-28     7 recovered
    ## 2288         Hainan Mainland China 19.19590 109.7453 2020-02-28     2 recovered
    ## 2289          Hebei Mainland China 38.04280 114.5149 2020-02-28     3 recovered
    ## 2290   Heilongjiang Mainland China 47.86200 127.7615 2020-02-28    13 recovered
    ## 2291          Henan Mainland China 33.88202 113.6140 2020-02-28    44 recovered
    ## 2293          Hubei Mainland China 30.97560 112.2707 2020-02-28  3020 recovered
    ## 2294          Hunan Mainland China 27.61040 111.7088 2020-02-28    26 recovered
    ## 2295 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-28     2 recovered
    ## 2296        Jiangsu Mainland China 32.97110 119.4550 2020-02-28    17 recovered
    ## 2297        Jiangxi Mainland China 27.61400 115.7221 2020-02-28    36 recovered
    ## 2298          Jilin Mainland China 43.66610 126.1923 2020-02-28     6 recovered
    ## 2299        Shaanxi Mainland China 35.19170 108.8701 2020-02-28     4 recovered
    ## 2300       Shandong Mainland China 36.34270 118.1498 2020-02-28    18 recovered
    ## 2301       Shanghai Mainland China 31.20200 121.4491 2020-02-28     3 recovered
    ## 2302         Shanxi Mainland China 37.57770 112.2922 2020-02-28     5 recovered
    ## 2303        Sichuan Mainland China 30.61710 102.7103 2020-02-28    17 recovered
    ## 2305       Xinjiang Mainland China 41.11290  85.2401 2020-02-28     9 recovered
    ## 2306         Yunnan Mainland China 24.97400 101.4870 2020-02-28     6 recovered
    ## 2307       Zhejiang Mainland China 29.18320 120.0934 2020-02-28    43 recovered
    ## 2340        Beijing Mainland China 40.18240 116.4142 2020-02-29     1 confirmed
    ## 2343      Guangdong Mainland China 23.34170 113.4244 2020-02-29     1 confirmed
    ## 2345          Hubei Mainland China 30.97560 112.2707 2020-02-29   423 confirmed
    ## 2346          Hunan Mainland China 27.61040 111.7088 2020-02-29     1 confirmed
    ## 2348        Ningxia Mainland China 37.26920 106.1655 2020-02-29     1 confirmed
    ## 2362        Beijing Mainland China 40.18240 116.4142 2020-02-29     1     death
    ## 2363          Henan Mainland China 33.88202 113.6140 2020-02-29     1     death
    ## 2364          Hubei Mainland China 30.97560 112.2707 2020-02-29    45     death
    ## 2372          Anhui Mainland China 31.82570 117.2264 2020-02-29    47 recovered
    ## 2373        Beijing Mainland China 40.18240 116.4142 2020-02-29    14 recovered
    ## 2374      Chongqing Mainland China 30.05720 107.8740 2020-02-29    16 recovered
    ## 2375         Fujian Mainland China 26.07890 117.9874 2020-02-29     8 recovered
    ## 2376      Guangdong Mainland China 23.34170 113.4244 2020-02-29    48 recovered
    ## 2377        Guangxi Mainland China 23.82980 108.7881 2020-02-29     8 recovered
    ## 2378         Hainan Mainland China 19.19590 109.7453 2020-02-29    15 recovered
    ## 2379          Hebei Mainland China 38.04280 114.5149 2020-02-29     5 recovered
    ## 2380   Heilongjiang Mainland China 47.86200 127.7615 2020-02-29    18 recovered
    ## 2381          Henan Mainland China 33.88202 113.6140 2020-02-29    58 recovered
    ## 2383          Hubei Mainland China 30.97560 112.2707 2020-02-29  2590 recovered
    ## 2384          Hunan Mainland China 27.61040 111.7088 2020-02-29    16 recovered
    ## 2385 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-29     4 recovered
    ## 2386        Jiangsu Mainland China 32.97110 119.4550 2020-02-29     8 recovered
    ## 2387        Jiangxi Mainland China 27.61400 115.7221 2020-02-29    21 recovered
    ## 2388          Jilin Mainland China 43.66610 126.1923 2020-02-29     2 recovered
    ## 2389       Liaoning Mainland China 41.29560 122.6085 2020-02-29     3 recovered
    ## 2390        Ningxia Mainland China 37.26920 106.1655 2020-02-29     1 recovered
    ## 2391        Shaanxi Mainland China 35.19170 108.8701 2020-02-29     8 recovered
    ## 2392       Shandong Mainland China 36.34270 118.1498 2020-02-29    16 recovered
    ## 2393       Shanghai Mainland China 31.20200 121.4491 2020-02-29     8 recovered
    ## 2394         Shanxi Mainland China 37.57770 112.2922 2020-02-29     2 recovered
    ## 2395        Sichuan Mainland China 30.61710 102.7103 2020-02-29    13 recovered
    ## 2397        Tianjin Mainland China 39.30540 117.3230 2020-02-29     7 recovered
    ## 2398       Xinjiang Mainland China 41.11290  85.2401 2020-02-29    10 recovered
    ## 2399         Yunnan Mainland China 24.97400 101.4870 2020-02-29     1 recovered
    ## 2400       Zhejiang Mainland China 29.18320 120.0934 2020-02-29    41 recovered
    ## 2435        Beijing Mainland China 40.18240 116.4142 2020-03-01     2 confirmed
    ## 2438          Hubei Mainland China 30.97560 112.2707 2020-03-01   570 confirmed
    ## 2440       Liaoning Mainland China 41.29560 122.6085 2020-03-01     1 confirmed
    ## 2443       Shandong Mainland China 36.34270 118.1498 2020-03-01     2 confirmed
    ## 2452          Henan Mainland China 33.88202 113.6140 2020-03-01     1     death
    ## 2453          Hubei Mainland China 30.97560 112.2707 2020-03-01    34     death
    ## 2458          Anhui Mainland China 31.82570 117.2264 2020-03-01     5 recovered
    ## 2459        Beijing Mainland China 40.18240 116.4142 2020-03-01     5 recovered
    ## 2460      Chongqing Mainland China 30.05720 107.8740 2020-03-01    12 recovered
    ## 2461         Fujian Mainland China 26.07890 117.9874 2020-03-01     4 recovered
    ## 2462          Gansu Mainland China 36.06110 103.8343 2020-03-01     2 recovered
    ## 2463      Guangdong Mainland China 23.34170 113.4244 2020-03-01    33 recovered
    ## 2464        Guangxi Mainland China 23.82980 108.7881 2020-03-01     5 recovered
    ## 2465         Hainan Mainland China 19.19590 109.7453 2020-03-01     1 recovered
    ## 2466          Hebei Mainland China 38.04280 114.5149 2020-03-01    12 recovered
    ## 2467   Heilongjiang Mainland China 47.86200 127.7615 2020-03-01    41 recovered
    ## 2468          Henan Mainland China 33.88202 113.6140 2020-03-01    28 recovered
    ## 2470          Hubei Mainland China 30.97560 112.2707 2020-03-01  2543 recovered
    ## 2471          Hunan Mainland China 27.61040 111.7088 2020-03-01    20 recovered
    ## 2472 Inner Mongolia Mainland China 44.09350 113.9448 2020-03-01     3 recovered
    ## 2473        Jiangsu Mainland China 32.97110 119.4550 2020-03-01    13 recovered
    ## 2474        Jiangxi Mainland China 27.61400 115.7221 2020-03-01    20 recovered
    ## 2475          Jilin Mainland China 43.66610 126.1923 2020-03-01     3 recovered
    ## 2476       Liaoning Mainland China 41.29560 122.6085 2020-03-01     7 recovered
    ## 2477        Shaanxi Mainland China 35.19170 108.8701 2020-03-01     1 recovered
    ## 2478       Shandong Mainland China 36.34270 118.1498 2020-03-01    22 recovered
    ## 2479       Shanghai Mainland China 31.20200 121.4491 2020-03-01     3 recovered
    ## 2480         Shanxi Mainland China 37.57770 112.2922 2020-03-01     2 recovered
    ## 2481        Sichuan Mainland China 30.61710 102.7103 2020-03-01    14 recovered
    ## 2482        Tianjin Mainland China 39.30540 117.3230 2020-03-01     2 recovered
    ## 2483       Xinjiang Mainland China 41.11290  85.2401 2020-03-01     2 recovered
    ## 2484         Yunnan Mainland China 24.97400 101.4870 2020-03-01     6 recovered
    ## 2485       Zhejiang Mainland China 29.18320 120.0934 2020-03-01    30 recovered
    ## 2518        Beijing Mainland China 40.18240 116.4142 2020-03-02     1 confirmed
    ## 2521      Guangdong Mainland China 23.34170 113.4244 2020-03-02     1 confirmed
    ## 2524          Hubei Mainland China 30.97560 112.2707 2020-03-02   196 confirmed
    ## 2527        Ningxia Mainland China 37.26920 106.1655 2020-03-02     1 confirmed
    ## 2542       Zhejiang Mainland China 29.18320 120.0934 2020-03-02     1 confirmed
    ## 2547          Hubei Mainland China 30.97560 112.2707 2020-03-02    42     death
    ## 2554          Anhui Mainland China 31.82570 117.2264 2020-03-02    44 recovered
    ## 2555        Beijing Mainland China 40.18240 116.4142 2020-03-02     6 recovered
    ## 2556      Chongqing Mainland China 30.05720 107.8740 2020-03-02    19 recovered
    ## 2557         Fujian Mainland China 26.07890 117.9874 2020-03-02     8 recovered
    ## 2558          Gansu Mainland China 36.06110 103.8343 2020-03-02     1 recovered
    ## 2559      Guangdong Mainland China 23.34170 113.4244 2020-03-02    43 recovered
    ## 2560        Guangxi Mainland China 23.82980 108.7881 2020-03-02    11 recovered
    ## 2561        Guizhou Mainland China 26.81540 106.8748 2020-03-02     2 recovered
    ## 2562         Hainan Mainland China 19.19590 109.7453 2020-03-02     2 recovered
    ## 2563          Hebei Mainland China 38.04280 114.5149 2020-03-02     2 recovered
    ## 2564   Heilongjiang Mainland China 47.86200 127.7615 2020-03-02    14 recovered
    ## 2565          Henan Mainland China 33.88202 113.6140 2020-03-02     7 recovered
    ## 2566          Hubei Mainland China 30.97560 112.2707 2020-03-02  2398 recovered
    ## 2567          Hunan Mainland China 27.61040 111.7088 2020-03-02    21 recovered
    ## 2568 Inner Mongolia Mainland China 44.09350 113.9448 2020-03-02     2 recovered
    ## 2569        Jiangsu Mainland China 32.97110 119.4550 2020-03-02     7 recovered
    ## 2570        Jiangxi Mainland China 27.61400 115.7221 2020-03-02    19 recovered
    ## 2571          Jilin Mainland China 43.66610 126.1923 2020-03-02     5 recovered
    ## 2572        Shaanxi Mainland China 35.19170 108.8701 2020-03-02     8 recovered
    ## 2573       Shandong Mainland China 36.34270 118.1498 2020-03-02    17 recovered
    ## 2574       Shanghai Mainland China 31.20200 121.4491 2020-03-02     2 recovered
    ## 2575         Shanxi Mainland China 37.57770 112.2922 2020-03-02     3 recovered
    ## 2576        Sichuan Mainland China 30.61710 102.7103 2020-03-02    21 recovered
    ## 2578       Xinjiang Mainland China 41.11290  85.2401 2020-03-02     2 recovered
    ## 2579         Yunnan Mainland China 24.97400 101.4870 2020-03-02     5 recovered
    ## 2580       Zhejiang Mainland China 29.18320 120.0934 2020-03-02    23 recovered
    ## 2624          Hubei Mainland China 30.97560 112.2707 2020-03-03   114 confirmed
    ## 2626       Liaoning Mainland China 41.29560 122.6085 2020-03-03     3 confirmed
    ## 2632       Shanghai Mainland China 31.20200 121.4491 2020-03-03     1 confirmed
    ## 2638       Zhejiang Mainland China 29.18320 120.0934 2020-03-03     7 confirmed
    ## 2644          Hubei Mainland China 30.97560 112.2707 2020-03-03    32     death
    ## 2645 Inner Mongolia Mainland China 44.09350 113.9448 2020-03-03     1     death
    ## 2653          Anhui Mainland China 31.82570 117.2264 2020-03-03    19 recovered
    ## 2654        Beijing Mainland China 40.18240 116.4142 2020-03-03     6 recovered
    ## 2655      Chongqing Mainland China 30.05720 107.8740 2020-03-03    21 recovered
    ## 2656         Fujian Mainland China 26.07890 117.9874 2020-03-03     5 recovered
    ## 2657          Gansu Mainland China 36.06110 103.8343 2020-03-03     1 recovered
    ## 2658      Guangdong Mainland China 23.34170 113.4244 2020-03-03    42 recovered
    ## 2659        Guangxi Mainland China 23.82980 108.7881 2020-03-03    10 recovered
    ## 2660         Hainan Mainland China 19.19590 109.7453 2020-03-03     4 recovered
    ## 2661          Hebei Mainland China 38.04280 114.5149 2020-03-03     4 recovered
    ## 2662   Heilongjiang Mainland China 47.86200 127.7615 2020-03-03    10 recovered
    ## 2663          Henan Mainland China 33.88202 113.6140 2020-03-03    26 recovered
    ## 2665          Hubei Mainland China 30.97560 112.2707 2020-03-03  2274 recovered
    ## 2666          Hunan Mainland China 27.61040 111.7088 2020-03-03    19 recovered
    ## 2667 Inner Mongolia Mainland China 44.09350 113.9448 2020-03-03     5 recovered
    ## 2668        Jiangsu Mainland China 32.97110 119.4550 2020-03-03    19 recovered
    ## 2669        Jiangxi Mainland China 27.61400 115.7221 2020-03-03    20 recovered
    ## 2670       Liaoning Mainland China 41.29560 122.6085 2020-03-03     3 recovered
    ## 2673       Shandong Mainland China 36.34270 118.1498 2020-03-03    51 recovered
    ## 2674       Shanghai Mainland China 31.20200 121.4491 2020-03-03     2 recovered
    ## 2675         Shanxi Mainland China 37.57770 112.2922 2020-03-03     5 recovered
    ## 2676        Sichuan Mainland China 30.61710 102.7103 2020-03-03     8 recovered
    ## 2677        Tianjin Mainland China 39.30540 117.3230 2020-03-03    13 recovered
    ## 2678       Xinjiang Mainland China 41.11290  85.2401 2020-03-03     2 recovered
    ## 2679         Yunnan Mainland China 24.97400 101.4870 2020-03-03     1 recovered
    ## 2680       Zhejiang Mainland China 29.18320 120.0934 2020-03-03    24 recovered
    ## 2724        Beijing Mainland China 40.18240 116.4142 2020-03-04     4 confirmed
    ## 2728          Hubei Mainland China 30.97560 112.2707 2020-03-04   115 confirmed
    ## 2732        Ningxia Mainland China 37.26920 106.1655 2020-03-04     1 confirmed
    ## 2745          Hubei Mainland China 30.97560 112.2707 2020-03-04    36     death
    ## 2755          Anhui Mainland China 31.82570 117.2264 2020-03-04    20 recovered
    ## 2756        Beijing Mainland China 40.18240 116.4142 2020-03-04     9 recovered
    ## 2757      Chongqing Mainland China 30.05720 107.8740 2020-03-04    12 recovered
    ## 2758         Fujian Mainland China 26.07890 117.9874 2020-03-04    10 recovered
    ## 2759          Gansu Mainland China 36.06110 103.8343 2020-03-04     1 recovered
    ## 2760      Guangdong Mainland China 23.34170 113.4244 2020-03-04    32 recovered
    ## 2761        Guangxi Mainland China 23.82980 108.7881 2020-03-04     8 recovered
    ## 2762         Hainan Mainland China 19.19590 109.7453 2020-03-04     3 recovered
    ## 2763          Hebei Mainland China 38.04280 114.5149 2020-03-04     1 recovered
    ## 2764   Heilongjiang Mainland China 47.86200 127.7615 2020-03-04     7 recovered
    ## 2765          Henan Mainland China 33.88202 113.6140 2020-03-04     3 recovered
    ## 2766          Hubei Mainland China 30.97560 112.2707 2020-03-04  2349 recovered
    ## 2767          Hunan Mainland China 27.61040 111.7088 2020-03-04    10 recovered
    ## 2768 Inner Mongolia Mainland China 44.09350 113.9448 2020-03-04     4 recovered
    ## 2769        Jiangsu Mainland China 32.97110 119.4550 2020-03-04    15 recovered
    ## 2770        Jiangxi Mainland China 27.61400 115.7221 2020-03-04    14 recovered
    ## 2771          Jilin Mainland China 43.66610 126.1923 2020-03-04     3 recovered
    ## 2772        Shaanxi Mainland China 35.19170 108.8701 2020-03-04     7 recovered
    ## 2773       Shandong Mainland China 36.34270 118.1498 2020-03-04     5 recovered
    ## 2774       Shanghai Mainland China 31.20200 121.4491 2020-03-04     4 recovered
    ## 2775        Sichuan Mainland China 30.61710 102.7103 2020-03-04    12 recovered
    ## 2776       Xinjiang Mainland China 41.11290  85.2401 2020-03-04     1 recovered
    ## 2777       Zhejiang Mainland China 29.18320 120.0934 2020-03-04    21 recovered
    ## 2827          Gansu Mainland China 36.06110 103.8343 2020-03-05    11 confirmed
    ## 2829      Guangdong Mainland China 23.34170 113.4244 2020-03-05     1 confirmed
    ## 2831   Heilongjiang Mainland China 47.86200 127.7615 2020-03-05     1 confirmed
    ## 2832          Hubei Mainland China 30.97560 112.2707 2020-03-05   134 confirmed
    ## 2842       Shanghai Mainland China 31.20200 121.4491 2020-03-05     1 confirmed
    ## 2843        Sichuan Mainland China 30.61710 102.7103 2020-03-05     1 confirmed
    ## 2850       Zhejiang Mainland China 29.18320 120.0934 2020-03-05     2 confirmed
    ## 2857         Hainan Mainland China 19.19590 109.7453 2020-03-05     1     death
    ## 2858          Hubei Mainland China 30.97560 112.2707 2020-03-05    31     death
    ## 2862          Anhui Mainland China 31.82570 117.2264 2020-03-05    14 recovered
    ## 2863      Chongqing Mainland China 30.05720 107.8740 2020-03-05    10 recovered
    ## 2864         Fujian Mainland China 26.07890 117.9874 2020-03-05     7 recovered
    ## 2865      Guangdong Mainland China 23.34170 113.4244 2020-03-05    48 recovered
    ## 2866        Guangxi Mainland China 23.82980 108.7881 2020-03-05     4 recovered
    ## 2867          Hebei Mainland China 38.04280 114.5149 2020-03-05     3 recovered
    ## 2868   Heilongjiang Mainland China 47.86200 127.7615 2020-03-05     6 recovered
    ## 2869          Henan Mainland China 33.88202 113.6140 2020-03-05     5 recovered
    ## 2871          Hubei Mainland China 30.97560 112.2707 2020-03-05  2035 recovered
    ## 2872          Hunan Mainland China 27.61040 111.7088 2020-03-05    22 recovered
    ## 2873 Inner Mongolia Mainland China 44.09350 113.9448 2020-03-05     2 recovered
    ## 2874        Jiangsu Mainland China 32.97110 119.4550 2020-03-05     6 recovered
    ## 2875        Jiangxi Mainland China 27.61400 115.7221 2020-03-05    17 recovered
    ## 2876          Jilin Mainland China 43.66610 126.1923 2020-03-05     2 recovered
    ## 2878        Shaanxi Mainland China 35.19170 108.8701 2020-03-05     1 recovered
    ## 2879       Shandong Mainland China 36.34270 118.1498 2020-03-05    62 recovered
    ## 2880       Shanghai Mainland China 31.20200 121.4491 2020-03-05     5 recovered
    ## 2881         Shanxi Mainland China 37.57770 112.2922 2020-03-05     2 recovered
    ## 2882        Sichuan Mainland China 30.61710 102.7103 2020-03-05    19 recovered
    ## 2883        Tianjin Mainland China 39.30540 117.3230 2020-03-05     4 recovered
    ## 2885       Xinjiang Mainland China 41.11290  85.2401 2020-03-05     1 recovered
    ## 2886       Zhejiang Mainland China 29.18320 120.0934 2020-03-05    10 recovered

``` r
mainland_2 <-  virus[virus$Country.Region == "Mainland China",]
mainland_2
```

    ##      Province.State Country.Region      Lat     Long       date cases      type
    ## 4             Anhui Mainland China 31.82570 117.2264 2020-01-22     1 confirmed
    ## 5           Beijing Mainland China 40.18240 116.4142 2020-01-22    14 confirmed
    ## 6         Chongqing Mainland China 30.05720 107.8740 2020-01-22     6 confirmed
    ## 7            Fujian Mainland China 26.07890 117.9874 2020-01-22     1 confirmed
    ## 8         Guangdong Mainland China 23.34170 113.4244 2020-01-22    26 confirmed
    ## 9           Guangxi Mainland China 23.82980 108.7881 2020-01-22     2 confirmed
    ## 10          Guizhou Mainland China 26.81540 106.8748 2020-01-22     1 confirmed
    ## 11           Hainan Mainland China 19.19590 109.7453 2020-01-22     4 confirmed
    ## 12            Hebei Mainland China 38.04280 114.5149 2020-01-22     1 confirmed
    ## 13            Henan Mainland China 33.88202 113.6140 2020-01-22     5 confirmed
    ## 14            Hubei Mainland China 30.97560 112.2707 2020-01-22   444 confirmed
    ## 15            Hunan Mainland China 27.61040 111.7088 2020-01-22     4 confirmed
    ## 16          Jiangsu Mainland China 32.97110 119.4550 2020-01-22     1 confirmed
    ## 17          Jiangxi Mainland China 27.61400 115.7221 2020-01-22     2 confirmed
    ## 19         Liaoning Mainland China 41.29560 122.6085 2020-01-22     2 confirmed
    ## 21          Ningxia Mainland China 37.26920 106.1655 2020-01-22     1 confirmed
    ## 22         Shandong Mainland China 36.34270 118.1498 2020-01-22     2 confirmed
    ## 23         Shanghai Mainland China 31.20200 121.4491 2020-01-22     9 confirmed
    ## 24           Shanxi Mainland China 37.57770 112.2922 2020-01-22     1 confirmed
    ## 25          Sichuan Mainland China 30.61710 102.7103 2020-01-22     5 confirmed
    ## 27          Tianjin Mainland China 39.30540 117.3230 2020-01-22     4 confirmed
    ## 28           Yunnan Mainland China 24.97400 101.4870 2020-01-22     1 confirmed
    ## 29         Zhejiang Mainland China 29.18320 120.0934 2020-01-22    10 confirmed
    ## 30            Hubei Mainland China 30.97560 112.2707 2020-01-22    17     death
    ## 31            Hubei Mainland China 30.97560 112.2707 2020-01-22    28 recovered
    ## 36            Anhui Mainland China 31.82570 117.2264 2020-01-23     8 confirmed
    ## 37          Beijing Mainland China 40.18240 116.4142 2020-01-23     8 confirmed
    ## 38        Chongqing Mainland China 30.05720 107.8740 2020-01-23     3 confirmed
    ## 39           Fujian Mainland China 26.07890 117.9874 2020-01-23     4 confirmed
    ## 40            Gansu Mainland China 36.06110 103.8343 2020-01-23     2 confirmed
    ## 41        Guangdong Mainland China 23.34170 113.4244 2020-01-23     6 confirmed
    ## 42          Guangxi Mainland China 23.82980 108.7881 2020-01-23     3 confirmed
    ## 43          Guizhou Mainland China 26.81540 106.8748 2020-01-23     2 confirmed
    ## 44           Hainan Mainland China 19.19590 109.7453 2020-01-23     1 confirmed
    ## 45     Heilongjiang Mainland China 47.86200 127.7615 2020-01-23     2 confirmed
    ## 47            Hunan Mainland China 27.61040 111.7088 2020-01-23     5 confirmed
    ## 48          Jiangsu Mainland China 32.97110 119.4550 2020-01-23     4 confirmed
    ## 49          Jiangxi Mainland China 27.61400 115.7221 2020-01-23     5 confirmed
    ## 50            Jilin Mainland China 43.66610 126.1923 2020-01-23     1 confirmed
    ## 51         Liaoning Mainland China 41.29560 122.6085 2020-01-23     1 confirmed
    ## 53          Shaanxi Mainland China 35.19170 108.8701 2020-01-23     3 confirmed
    ## 54         Shandong Mainland China 36.34270 118.1498 2020-01-23     4 confirmed
    ## 55         Shanghai Mainland China 31.20200 121.4491 2020-01-23     7 confirmed
    ## 56          Sichuan Mainland China 30.61710 102.7103 2020-01-23     3 confirmed
    ## 57         Xinjiang Mainland China 41.11290  85.2401 2020-01-23     2 confirmed
    ## 58           Yunnan Mainland China 24.97400 101.4870 2020-01-23     1 confirmed
    ## 59         Zhejiang Mainland China 29.18320 120.0934 2020-01-23    17 confirmed
    ## 60            Hebei Mainland China 38.04280 114.5149 2020-01-23     1     death
    ## 61        Guangdong Mainland China 23.34170 113.4244 2020-01-23     2 recovered
    ## 67            Anhui Mainland China 31.82570 117.2264 2020-01-24     6 confirmed
    ## 68          Beijing Mainland China 40.18240 116.4142 2020-01-24    14 confirmed
    ## 69        Chongqing Mainland China 30.05720 107.8740 2020-01-24    18 confirmed
    ## 71           Fujian Mainland China 26.07890 117.9874 2020-01-24     5 confirmed
    ## 72        Guangdong Mainland China 23.34170 113.4244 2020-01-24    21 confirmed
    ## 73          Guangxi Mainland China 23.82980 108.7881 2020-01-24    18 confirmed
    ## 74           Hainan Mainland China 19.19590 109.7453 2020-01-24     3 confirmed
    ## 75            Hebei Mainland China 38.04280 114.5149 2020-01-24     1 confirmed
    ## 76     Heilongjiang Mainland China 47.86200 127.7615 2020-01-24     2 confirmed
    ## 77            Henan Mainland China 33.88202 113.6140 2020-01-24     4 confirmed
    ## 78            Hubei Mainland China 30.97560 112.2707 2020-01-24   105 confirmed
    ## 79            Hunan Mainland China 27.61040 111.7088 2020-01-24    15 confirmed
    ## 80   Inner Mongolia Mainland China 44.09350 113.9448 2020-01-24     1 confirmed
    ## 81          Jiangsu Mainland China 32.97110 119.4550 2020-01-24     4 confirmed
    ## 82          Jiangxi Mainland China 27.61400 115.7221 2020-01-24    11 confirmed
    ## 83            Jilin Mainland China 43.66610 126.1923 2020-01-24     2 confirmed
    ## 84         Liaoning Mainland China 41.29560 122.6085 2020-01-24     1 confirmed
    ## 85          Ningxia Mainland China 37.26920 106.1655 2020-01-24     1 confirmed
    ## 86          Shaanxi Mainland China 35.19170 108.8701 2020-01-24     2 confirmed
    ## 87         Shandong Mainland China 36.34270 118.1498 2020-01-24     9 confirmed
    ## 88         Shanghai Mainland China 31.20200 121.4491 2020-01-24     4 confirmed
    ## 89          Sichuan Mainland China 30.61710 102.7103 2020-01-24     7 confirmed
    ## 91          Tianjin Mainland China 39.30540 117.3230 2020-01-24     4 confirmed
    ## 92           Yunnan Mainland China 24.97400 101.4870 2020-01-24     3 confirmed
    ## 93         Zhejiang Mainland China 29.18320 120.0934 2020-01-24    16 confirmed
    ## 94     Heilongjiang Mainland China 47.86200 127.7615 2020-01-24     1     death
    ## 95            Hubei Mainland China 30.97560 112.2707 2020-01-24     7     death
    ## 96          Beijing Mainland China 40.18240 116.4142 2020-01-24     1 recovered
    ## 97            Hubei Mainland China 30.97560 112.2707 2020-01-24     3 recovered
    ## 98         Shanghai Mainland China 31.20200 121.4491 2020-01-24     1 recovered
    ## 99         Zhejiang Mainland China 29.18320 120.0934 2020-01-24     1 recovered
    ## 104           Anhui Mainland China 31.82570 117.2264 2020-01-25    24 confirmed
    ## 105         Beijing Mainland China 40.18240 116.4142 2020-01-25     5 confirmed
    ## 106       Chongqing Mainland China 30.05720 107.8740 2020-01-25    30 confirmed
    ## 107          Fujian Mainland China 26.07890 117.9874 2020-01-25     8 confirmed
    ## 108           Gansu Mainland China 36.06110 103.8343 2020-01-25     2 confirmed
    ## 109       Guangdong Mainland China 23.34170 113.4244 2020-01-25    25 confirmed
    ## 110         Guizhou Mainland China 26.81540 106.8748 2020-01-25     1 confirmed
    ## 111          Hainan Mainland China 19.19590 109.7453 2020-01-25    11 confirmed
    ## 112           Hebei Mainland China 38.04280 114.5149 2020-01-25     6 confirmed
    ## 113    Heilongjiang Mainland China 47.86200 127.7615 2020-01-25     5 confirmed
    ## 114           Henan Mainland China 33.88202 113.6140 2020-01-25    23 confirmed
    ## 116           Hubei Mainland China 30.97560 112.2707 2020-01-25   212 confirmed
    ## 117           Hunan Mainland China 27.61040 111.7088 2020-01-25    19 confirmed
    ## 118  Inner Mongolia Mainland China 44.09350 113.9448 2020-01-25     6 confirmed
    ## 119         Jiangsu Mainland China 32.97110 119.4550 2020-01-25     9 confirmed
    ## 120           Jilin Mainland China 43.66610 126.1923 2020-01-25     1 confirmed
    ## 121        Liaoning Mainland China 41.29560 122.6085 2020-01-25    13 confirmed
    ## 122         Ningxia Mainland China 37.26920 106.1655 2020-01-25     1 confirmed
    ## 123         Qinghai Mainland China 35.74520  95.9956 2020-01-25     1 confirmed
    ## 124         Shaanxi Mainland China 35.19170 108.8701 2020-01-25    10 confirmed
    ## 125        Shandong Mainland China 36.34270 118.1498 2020-01-25    12 confirmed
    ## 126        Shanghai Mainland China 31.20200 121.4491 2020-01-25    13 confirmed
    ## 127          Shanxi Mainland China 37.57770 112.2922 2020-01-25     5 confirmed
    ## 128         Sichuan Mainland China 30.61710 102.7103 2020-01-25    13 confirmed
    ## 129         Tianjin Mainland China 39.30540 117.3230 2020-01-25     2 confirmed
    ## 130        Xinjiang Mainland China 41.11290  85.2401 2020-01-25     1 confirmed
    ## 131          Yunnan Mainland China 24.97400 101.4870 2020-01-25     6 confirmed
    ## 132        Zhejiang Mainland China 29.18320 120.0934 2020-01-25    19 confirmed
    ## 133           Hubei Mainland China 30.97560 112.2707 2020-01-25    16     death
    ## 134         Beijing Mainland China 40.18240 116.4142 2020-01-25     1 recovered
    ## 135           Hubei Mainland China 30.97560 112.2707 2020-01-25     1 recovered
    ## 136         Jiangsu Mainland China 32.97110 119.4550 2020-01-25     1 recovered
    ## 142           Anhui Mainland China 31.82570 117.2264 2020-01-26    21 confirmed
    ## 143         Beijing Mainland China 40.18240 116.4142 2020-01-26    27 confirmed
    ## 144       Chongqing Mainland China 30.05720 107.8740 2020-01-26    18 confirmed
    ## 145          Fujian Mainland China 26.07890 117.9874 2020-01-26    17 confirmed
    ## 146           Gansu Mainland China 36.06110 103.8343 2020-01-26     3 confirmed
    ## 147       Guangdong Mainland China 23.34170 113.4244 2020-01-26    33 confirmed
    ## 148         Guangxi Mainland China 23.82980 108.7881 2020-01-26    13 confirmed
    ## 149         Guizhou Mainland China 26.81540 106.8748 2020-01-26     1 confirmed
    ## 150          Hainan Mainland China 19.19590 109.7453 2020-01-26     3 confirmed
    ## 151           Hebei Mainland China 38.04280 114.5149 2020-01-26     5 confirmed
    ## 152    Heilongjiang Mainland China 47.86200 127.7615 2020-01-26     6 confirmed
    ## 153           Henan Mainland China 33.88202 113.6140 2020-01-26    51 confirmed
    ## 155           Hubei Mainland China 30.97560 112.2707 2020-01-26   297 confirmed
    ## 156           Hunan Mainland China 27.61040 111.7088 2020-01-26    26 confirmed
    ## 157         Jiangsu Mainland China 32.97110 119.4550 2020-01-26    15 confirmed
    ## 158         Jiangxi Mainland China 27.61400 115.7221 2020-01-26    18 confirmed
    ## 159        Liaoning Mainland China 41.29560 122.6085 2020-01-26     4 confirmed
    ## 163         Ningxia Mainland China 37.26920 106.1655 2020-01-26     1 confirmed
    ## 165         Shaanxi Mainland China 35.19170 108.8701 2020-01-26     7 confirmed
    ## 166        Shandong Mainland China 36.34270 118.1498 2020-01-26    19 confirmed
    ## 167        Shanghai Mainland China 31.20200 121.4491 2020-01-26     7 confirmed
    ## 168          Shanxi Mainland China 37.57770 112.2922 2020-01-26     3 confirmed
    ## 169         Sichuan Mainland China 30.61710 102.7103 2020-01-26    16 confirmed
    ## 172         Tianjin Mainland China 39.30540 117.3230 2020-01-26     4 confirmed
    ## 175        Xinjiang Mainland China 41.11290  85.2401 2020-01-26     1 confirmed
    ## 176          Yunnan Mainland China 24.97400 101.4870 2020-01-26     5 confirmed
    ## 177        Zhejiang Mainland China 29.18320 120.0934 2020-01-26    42 confirmed
    ## 178           Henan Mainland China 33.88202 113.6140 2020-01-26     1     death
    ## 179           Hubei Mainland China 30.97560 112.2707 2020-01-26    12     death
    ## 180        Shanghai Mainland China 31.20200 121.4491 2020-01-26     1     death
    ## 183           Hubei Mainland China 30.97560 112.2707 2020-01-26    10 recovered
    ## 189           Anhui Mainland China 31.82570 117.2264 2020-01-27    10 confirmed
    ## 190         Beijing Mainland China 40.18240 116.4142 2020-01-27    12 confirmed
    ## 191       Chongqing Mainland China 30.05720 107.8740 2020-01-27    35 confirmed
    ## 192          Fujian Mainland China 26.07890 117.9874 2020-01-27    24 confirmed
    ## 193           Gansu Mainland China 36.06110 103.8343 2020-01-27     7 confirmed
    ## 194       Guangdong Mainland China 23.34170 113.4244 2020-01-27    40 confirmed
    ## 195         Guangxi Mainland China 23.82980 108.7881 2020-01-27    10 confirmed
    ## 196         Guizhou Mainland China 26.81540 106.8748 2020-01-27     2 confirmed
    ## 197          Hainan Mainland China 19.19590 109.7453 2020-01-27    11 confirmed
    ## 198           Hebei Mainland China 38.04280 114.5149 2020-01-27     5 confirmed
    ## 199    Heilongjiang Mainland China 47.86200 127.7615 2020-01-27     6 confirmed
    ## 200           Henan Mainland China 33.88202 113.6140 2020-01-27    45 confirmed
    ## 201           Hubei Mainland China 30.97560 112.2707 2020-01-27   365 confirmed
    ## 202           Hunan Mainland China 27.61040 111.7088 2020-01-27    31 confirmed
    ## 203  Inner Mongolia Mainland China 44.09350 113.9448 2020-01-27     4 confirmed
    ## 204         Jiangsu Mainland China 32.97110 119.4550 2020-01-27    14 confirmed
    ## 205         Jiangxi Mainland China 27.61400 115.7221 2020-01-27    36 confirmed
    ## 206           Jilin Mainland China 43.66610 126.1923 2020-01-27     2 confirmed
    ## 207        Liaoning Mainland China 41.29560 122.6085 2020-01-27     6 confirmed
    ## 210         Ningxia Mainland China 37.26920 106.1655 2020-01-27     3 confirmed
    ## 211         Qinghai Mainland China 35.74520  95.9956 2020-01-27     5 confirmed
    ## 212         Shaanxi Mainland China 35.19170 108.8701 2020-01-27    13 confirmed
    ## 213        Shandong Mainland China 36.34270 118.1498 2020-01-27    29 confirmed
    ## 214        Shanghai Mainland China 31.20200 121.4491 2020-01-27    13 confirmed
    ## 215          Shanxi Mainland China 37.57770 112.2922 2020-01-27     4 confirmed
    ## 216         Sichuan Mainland China 30.61710 102.7103 2020-01-27    25 confirmed
    ## 218         Tianjin Mainland China 39.30540 117.3230 2020-01-27     9 confirmed
    ## 219        Xinjiang Mainland China 41.11290  85.2401 2020-01-27     1 confirmed
    ## 220          Yunnan Mainland China 24.97400 101.4870 2020-01-27    10 confirmed
    ## 221        Zhejiang Mainland China 29.18320 120.0934 2020-01-27    24 confirmed
    ## 222         Beijing Mainland China 40.18240 116.4142 2020-01-27     1     death
    ## 223          Hainan Mainland China 19.19590 109.7453 2020-01-27     1     death
    ## 224           Hubei Mainland China 30.97560 112.2707 2020-01-27    24     death
    ## 225       Guangdong Mainland China 23.34170 113.4244 2020-01-27     2 recovered
    ## 226           Hubei Mainland China 30.97560 112.2707 2020-01-27     3 recovered
    ## 227         Jiangxi Mainland China 27.61400 115.7221 2020-01-27     2 recovered
    ## 228        Shanghai Mainland China 31.20200 121.4491 2020-01-27     2 recovered
    ## 234           Anhui Mainland China 31.82570 117.2264 2020-01-28    36 confirmed
    ## 235         Beijing Mainland China 40.18240 116.4142 2020-01-28    11 confirmed
    ## 237       Chongqing Mainland China 30.05720 107.8740 2020-01-28    22 confirmed
    ## 238          Fujian Mainland China 26.07890 117.9874 2020-01-28    21 confirmed
    ## 239           Gansu Mainland China 36.06110 103.8343 2020-01-28     5 confirmed
    ## 240       Guangdong Mainland China 23.34170 113.4244 2020-01-28    56 confirmed
    ## 241         Guangxi Mainland China 23.82980 108.7881 2020-01-28     5 confirmed
    ## 242         Guizhou Mainland China 26.81540 106.8748 2020-01-28     2 confirmed
    ## 243          Hainan Mainland China 19.19590 109.7453 2020-01-28     7 confirmed
    ## 244           Hebei Mainland China 38.04280 114.5149 2020-01-28    15 confirmed
    ## 245    Heilongjiang Mainland China 47.86200 127.7615 2020-01-28    12 confirmed
    ## 246           Henan Mainland China 33.88202 113.6140 2020-01-28    40 confirmed
    ## 247           Hubei Mainland China 30.97560 112.2707 2020-01-28  2131 confirmed
    ## 248           Hunan Mainland China 27.61040 111.7088 2020-01-28    43 confirmed
    ## 249  Inner Mongolia Mainland China 44.09350 113.9448 2020-01-28     4 confirmed
    ## 250         Jiangsu Mainland China 32.97110 119.4550 2020-01-28    23 confirmed
    ## 251         Jiangxi Mainland China 27.61400 115.7221 2020-01-28    37 confirmed
    ## 252           Jilin Mainland China 43.66610 126.1923 2020-01-28     2 confirmed
    ## 253        Liaoning Mainland China 41.29560 122.6085 2020-01-28     7 confirmed
    ## 255         Ningxia Mainland China 37.26920 106.1655 2020-01-28     4 confirmed
    ## 256         Shaanxi Mainland China 35.19170 108.8701 2020-01-28    11 confirmed
    ## 257        Shandong Mainland China 36.34270 118.1498 2020-01-28    20 confirmed
    ## 258        Shanghai Mainland China 31.20200 121.4491 2020-01-28    13 confirmed
    ## 259          Shanxi Mainland China 37.57770 112.2922 2020-01-28    14 confirmed
    ## 260         Sichuan Mainland China 30.61710 102.7103 2020-01-28    21 confirmed
    ## 262         Tianjin Mainland China 39.30540 117.3230 2020-01-28     1 confirmed
    ## 263        Xinjiang Mainland China 41.11290  85.2401 2020-01-28     5 confirmed
    ## 264          Yunnan Mainland China 24.97400 101.4870 2020-01-28    18 confirmed
    ## 265        Zhejiang Mainland China 29.18320 120.0934 2020-01-28    45 confirmed
    ## 266           Hubei Mainland China 30.97560 112.2707 2020-01-28    49     death
    ## 268         Beijing Mainland China 40.18240 116.4142 2020-01-28     2 recovered
    ## 269         Guangxi Mainland China 23.82980 108.7881 2020-01-28     2 recovered
    ## 270           Hubei Mainland China 30.97560 112.2707 2020-01-28    35 recovered
    ## 271         Jiangxi Mainland China 27.61400 115.7221 2020-01-28     1 recovered
    ## 272        Shanghai Mainland China 31.20200 121.4491 2020-01-28     1 recovered
    ## 273        Zhejiang Mainland China 29.18320 120.0934 2020-01-28     2 recovered
    ## 278           Anhui Mainland China 31.82570 117.2264 2020-01-29    46 confirmed
    ## 279         Beijing Mainland China 40.18240 116.4142 2020-01-29    20 confirmed
    ## 280       Chongqing Mainland China 30.05720 107.8740 2020-01-29    15 confirmed
    ## 281          Fujian Mainland China 26.07890 117.9874 2020-01-29     4 confirmed
    ## 282           Gansu Mainland China 36.06110 103.8343 2020-01-29     5 confirmed
    ## 283       Guangdong Mainland China 23.34170 113.4244 2020-01-29    70 confirmed
    ## 284         Guangxi Mainland China 23.82980 108.7881 2020-01-29     7 confirmed
    ## 285          Hainan Mainland China 19.19590 109.7453 2020-01-29     3 confirmed
    ## 286           Hebei Mainland China 38.04280 114.5149 2020-01-29    15 confirmed
    ## 287    Heilongjiang Mainland China 47.86200 127.7615 2020-01-29     5 confirmed
    ## 288           Henan Mainland China 33.88202 113.6140 2020-01-29    38 confirmed
    ## 290           Hunan Mainland China 27.61040 111.7088 2020-01-29    78 confirmed
    ## 291  Inner Mongolia Mainland China 44.09350 113.9448 2020-01-29     1 confirmed
    ## 292         Jiangsu Mainland China 32.97110 119.4550 2020-01-29    29 confirmed
    ## 293           Jilin Mainland China 43.66610 126.1923 2020-01-29     1 confirmed
    ## 294        Liaoning Mainland China 41.29560 122.6085 2020-01-29     5 confirmed
    ## 295         Ningxia Mainland China 37.26920 106.1655 2020-01-29     1 confirmed
    ## 297         Shaanxi Mainland China 35.19170 108.8701 2020-01-29    10 confirmed
    ## 298        Shandong Mainland China 36.34270 118.1498 2020-01-29    35 confirmed
    ## 299        Shanghai Mainland China 31.20200 121.4491 2020-01-29    30 confirmed
    ## 300         Sichuan Mainland China 30.61710 102.7103 2020-01-29    18 confirmed
    ## 301         Tianjin Mainland China 39.30540 117.3230 2020-01-29     3 confirmed
    ## 302        Xinjiang Mainland China 41.11290  85.2401 2020-01-29     3 confirmed
    ## 303          Yunnan Mainland China 24.97400 101.4870 2020-01-29    11 confirmed
    ## 304        Zhejiang Mainland China 29.18320 120.0934 2020-01-29   123 confirmed
    ## 305           Henan Mainland China 33.88202 113.6140 2020-01-29     1     death
    ## 306         Sichuan Mainland China 30.61710 102.7103 2020-01-29     1     death
    ## 307           Anhui Mainland China 31.82570 117.2264 2020-01-29     2 recovered
    ## 308       Chongqing Mainland China 30.05720 107.8740 2020-01-29     1 recovered
    ## 309       Guangdong Mainland China 23.34170 113.4244 2020-01-29     1 recovered
    ## 310         Guizhou Mainland China 26.81540 106.8748 2020-01-29     1 recovered
    ## 311           Henan Mainland China 33.88202 113.6140 2020-01-29     1 recovered
    ## 312           Hubei Mainland China 30.97560 112.2707 2020-01-29     8 recovered
    ## 313        Liaoning Mainland China 41.29560 122.6085 2020-01-29     1 recovered
    ## 314        Shandong Mainland China 36.34270 118.1498 2020-01-29     1 recovered
    ## 315        Shanghai Mainland China 31.20200 121.4491 2020-01-29     1 recovered
    ## 316          Shanxi Mainland China 37.57770 112.2922 2020-01-29     1 recovered
    ## 317         Sichuan Mainland China 30.61710 102.7103 2020-01-29     1 recovered
    ## 323           Anhui Mainland China 31.82570 117.2264 2020-01-30    48 confirmed
    ## 324         Beijing Mainland China 40.18240 116.4142 2020-01-30     3 confirmed
    ## 325       Chongqing Mainland China 30.05720 107.8740 2020-01-30    35 confirmed
    ## 326          Fujian Mainland China 26.07890 117.9874 2020-01-30    17 confirmed
    ## 327           Gansu Mainland China 36.06110 103.8343 2020-01-30     2 confirmed
    ## 328       Guangdong Mainland China 23.34170 113.4244 2020-01-30    77 confirmed
    ## 329         Guangxi Mainland China 23.82980 108.7881 2020-01-30    20 confirmed
    ## 330         Guizhou Mainland China 26.81540 106.8748 2020-01-30     3 confirmed
    ## 331          Hainan Mainland China 19.19590 109.7453 2020-01-30     3 confirmed
    ## 332           Hebei Mainland China 38.04280 114.5149 2020-01-30    17 confirmed
    ## 333    Heilongjiang Mainland China 47.86200 127.7615 2020-01-30     6 confirmed
    ## 334           Henan Mainland China 33.88202 113.6140 2020-01-30    72 confirmed
    ## 335           Hubei Mainland China 30.97560 112.2707 2020-01-30  1349 confirmed
    ## 336           Hunan Mainland China 27.61040 111.7088 2020-01-30    56 confirmed
    ## 337  Inner Mongolia Mainland China 44.09350 113.9448 2020-01-30     3 confirmed
    ## 338         Jiangsu Mainland China 32.97110 119.4550 2020-01-30    30 confirmed
    ## 339         Jiangxi Mainland China 27.61400 115.7221 2020-01-30    53 confirmed
    ## 340           Jilin Mainland China 43.66610 126.1923 2020-01-30     5 confirmed
    ## 341        Liaoning Mainland China 41.29560 122.6085 2020-01-30     2 confirmed
    ## 342         Ningxia Mainland China 37.26920 106.1655 2020-01-30     5 confirmed
    ## 343         Qinghai Mainland China 35.74520  95.9956 2020-01-30     2 confirmed
    ## 345         Shaanxi Mainland China 35.19170 108.8701 2020-01-30     7 confirmed
    ## 346        Shandong Mainland China 36.34270 118.1498 2020-01-30    28 confirmed
    ## 347        Shanghai Mainland China 31.20200 121.4491 2020-01-30    16 confirmed
    ## 348          Shanxi Mainland China 37.57770 112.2922 2020-01-30     8 confirmed
    ## 349         Sichuan Mainland China 30.61710 102.7103 2020-01-30    34 confirmed
    ## 351         Tianjin Mainland China 39.30540 117.3230 2020-01-30     4 confirmed
    ## 352           Tibet Mainland China 31.69270  88.0924 2020-01-30     1 confirmed
    ## 354        Xinjiang Mainland China 41.11290  85.2401 2020-01-30     1 confirmed
    ## 355          Yunnan Mainland China 24.97400 101.4870 2020-01-30    15 confirmed
    ## 356        Zhejiang Mainland China 29.18320 120.0934 2020-01-30   132 confirmed
    ## 357    Heilongjiang Mainland China 47.86200 127.7615 2020-01-30     1     death
    ## 358           Hubei Mainland China 30.97560 112.2707 2020-01-30    37     death
    ## 359       Guangdong Mainland China 23.34170 113.4244 2020-01-30     5 recovered
    ## 360          Hainan Mainland China 19.19590 109.7453 2020-01-30     1 recovered
    ## 361           Henan Mainland China 33.88202 113.6140 2020-01-30     1 recovered
    ## 362           Hubei Mainland China 30.97560 112.2707 2020-01-30     2 recovered
    ## 363           Hunan Mainland China 27.61040 111.7088 2020-01-30     2 recovered
    ## 364         Jiangxi Mainland China 27.61400 115.7221 2020-01-30     2 recovered
    ## 365           Jilin Mainland China 43.66610 126.1923 2020-01-30     1 recovered
    ## 367        Zhejiang Mainland China 29.18320 120.0934 2020-01-30     1 recovered
    ## 377           Anhui Mainland China 31.82570 117.2264 2020-01-31    37 confirmed
    ## 378         Beijing Mainland China 40.18240 116.4142 2020-01-31    25 confirmed
    ## 379       Chongqing Mainland China 30.05720 107.8740 2020-01-31    29 confirmed
    ## 381          Fujian Mainland China 26.07890 117.9874 2020-01-31    19 confirmed
    ## 382           Gansu Mainland China 36.06110 103.8343 2020-01-31     3 confirmed
    ## 383       Guangdong Mainland China 23.34170 113.4244 2020-01-31    82 confirmed
    ## 384         Guangxi Mainland China 23.82980 108.7881 2020-01-31     9 confirmed
    ## 385         Guizhou Mainland China 26.81540 106.8748 2020-01-31    17 confirmed
    ## 386          Hainan Mainland China 19.19590 109.7453 2020-01-31     6 confirmed
    ## 387           Hebei Mainland China 38.04280 114.5149 2020-01-31    17 confirmed
    ## 388    Heilongjiang Mainland China 47.86200 127.7615 2020-01-31    15 confirmed
    ## 389           Henan Mainland China 33.88202 113.6140 2020-01-31    74 confirmed
    ## 391           Hubei Mainland China 30.97560 112.2707 2020-01-31   903 confirmed
    ## 392           Hunan Mainland China 27.61040 111.7088 2020-01-31    55 confirmed
    ## 393  Inner Mongolia Mainland China 44.09350 113.9448 2020-01-31     1 confirmed
    ## 394         Jiangsu Mainland China 32.97110 119.4550 2020-01-31    39 confirmed
    ## 395         Jiangxi Mainland China 27.61400 115.7221 2020-01-31    78 confirmed
    ## 396        Liaoning Mainland China 41.29560 122.6085 2020-01-31     7 confirmed
    ## 398         Ningxia Mainland China 37.26920 106.1655 2020-01-31     4 confirmed
    ## 401         Shaanxi Mainland China 35.19170 108.8701 2020-01-31    24 confirmed
    ## 402        Shandong Mainland China 36.34270 118.1498 2020-01-31    26 confirmed
    ## 403        Shanghai Mainland China 31.20200 121.4491 2020-01-31    23 confirmed
    ## 404          Shanxi Mainland China 37.57770 112.2922 2020-01-31     4 confirmed
    ## 405         Sichuan Mainland China 30.61710 102.7103 2020-01-31    35 confirmed
    ## 407         Tianjin Mainland China 39.30540 117.3230 2020-01-31     1 confirmed
    ## 410        Xinjiang Mainland China 41.11290  85.2401 2020-01-31     3 confirmed
    ## 411          Yunnan Mainland China 24.97400 101.4870 2020-01-31    13 confirmed
    ## 412        Zhejiang Mainland China 29.18320 120.0934 2020-01-31   110 confirmed
    ## 413           Hubei Mainland China 30.97560 112.2707 2020-01-31    42     death
    ## 414           Anhui Mainland China 31.82570 117.2264 2020-01-31     1 recovered
    ## 415         Beijing Mainland China 40.18240 116.4142 2020-01-31     1 recovered
    ## 416       Guangdong Mainland China 23.34170 113.4244 2020-01-31     1 recovered
    ## 417         Guizhou Mainland China 26.81540 106.8748 2020-01-31     1 recovered
    ## 418           Henan Mainland China 33.88202 113.6140 2020-01-31     1 recovered
    ## 419           Hubei Mainland China 30.97560 112.2707 2020-01-31    51 recovered
    ## 420  Inner Mongolia Mainland China 44.09350 113.9448 2020-01-31     1 recovered
    ## 421         Jiangsu Mainland China 32.97110 119.4550 2020-01-31     4 recovered
    ## 422         Jiangxi Mainland China 27.61400 115.7221 2020-01-31     2 recovered
    ## 423        Shandong Mainland China 36.34270 118.1498 2020-01-31     1 recovered
    ## 424        Shanghai Mainland China 31.20200 121.4491 2020-01-31     4 recovered
    ## 425          Yunnan Mainland China 24.97400 101.4870 2020-01-31     1 recovered
    ## 426        Zhejiang Mainland China 29.18320 120.0934 2020-01-31    10 recovered
    ## 434           Anhui Mainland China 31.82570 117.2264 2020-02-01    60 confirmed
    ## 435         Beijing Mainland China 40.18240 116.4142 2020-02-01    29 confirmed
    ## 437       Chongqing Mainland China 30.05720 107.8740 2020-02-01    36 confirmed
    ## 438          Fujian Mainland China 26.07890 117.9874 2020-02-01    24 confirmed
    ## 439           Gansu Mainland China 36.06110 103.8343 2020-02-01    11 confirmed
    ## 440       Guangdong Mainland China 23.34170 113.4244 2020-02-01    99 confirmed
    ## 441         Guangxi Mainland China 23.82980 108.7881 2020-02-01    13 confirmed
    ## 442          Hainan Mainland China 19.19590 109.7453 2020-02-01    10 confirmed
    ## 443           Hebei Mainland China 38.04280 114.5149 2020-02-01    14 confirmed
    ## 444    Heilongjiang Mainland China 47.86200 127.7615 2020-02-01    21 confirmed
    ## 445           Henan Mainland China 33.88202 113.6140 2020-02-01    70 confirmed
    ## 447           Hubei Mainland China 30.97560 112.2707 2020-02-01  1347 confirmed
    ## 448           Hunan Mainland China 27.61040 111.7088 2020-02-01    57 confirmed
    ## 449  Inner Mongolia Mainland China 44.09350 113.9448 2020-02-01     3 confirmed
    ## 450         Jiangsu Mainland China 32.97110 119.4550 2020-02-01    34 confirmed
    ## 451         Jiangxi Mainland China 27.61400 115.7221 2020-02-01    46 confirmed
    ## 452           Jilin Mainland China 43.66610 126.1923 2020-02-01     3 confirmed
    ## 453        Liaoning Mainland China 41.29560 122.6085 2020-02-01    16 confirmed
    ## 454         Ningxia Mainland China 37.26920 106.1655 2020-02-01     5 confirmed
    ## 455         Qinghai Mainland China 35.74520  95.9956 2020-02-01     1 confirmed
    ## 457         Shaanxi Mainland China 35.19170 108.8701 2020-02-01    14 confirmed
    ## 458        Shandong Mainland China 36.34270 118.1498 2020-02-01    22 confirmed
    ## 459        Shanghai Mainland China 31.20200 121.4491 2020-02-01    34 confirmed
    ## 460          Shanxi Mainland China 37.57770 112.2922 2020-02-01     8 confirmed
    ## 461         Sichuan Mainland China 30.61710 102.7103 2020-02-01    30 confirmed
    ## 463         Tianjin Mainland China 39.30540 117.3230 2020-02-01     9 confirmed
    ## 465        Xinjiang Mainland China 41.11290  85.2401 2020-02-01     1 confirmed
    ## 466          Yunnan Mainland China 24.97400 101.4870 2020-02-01    10 confirmed
    ## 467        Zhejiang Mainland China 29.18320 120.0934 2020-02-01    61 confirmed
    ## 468       Chongqing Mainland China 30.05720 107.8740 2020-02-01     1     death
    ## 469           Hubei Mainland China 30.97560 112.2707 2020-02-01    45     death
    ## 471           Anhui Mainland China 31.82570 117.2264 2020-02-01     2 recovered
    ## 472         Beijing Mainland China 40.18240 116.4142 2020-02-01     4 recovered
    ## 473       Chongqing Mainland China 30.05720 107.8740 2020-02-01     2 recovered
    ## 474       Guangdong Mainland China 23.34170 113.4244 2020-02-01     3 recovered
    ## 475    Heilongjiang Mainland China 47.86200 127.7615 2020-02-01     2 recovered
    ## 476           Hubei Mainland China 30.97560 112.2707 2020-02-01    27 recovered
    ## 477           Hunan Mainland China 27.61040 111.7088 2020-02-01     6 recovered
    ## 478         Jiangsu Mainland China 32.97110 119.4550 2020-02-01     1 recovered
    ## 479         Jiangxi Mainland China 27.61400 115.7221 2020-02-01     2 recovered
    ## 480        Shandong Mainland China 36.34270 118.1498 2020-02-01     1 recovered
    ## 481        Shanghai Mainland China 31.20200 121.4491 2020-02-01     1 recovered
    ## 482         Sichuan Mainland China 30.61710 102.7103 2020-02-01     2 recovered
    ## 483          Yunnan Mainland China 24.97400 101.4870 2020-02-01     1 recovered
    ## 484        Zhejiang Mainland China 29.18320 120.0934 2020-02-01     7 recovered
    ## 491           Anhui Mainland China 31.82570 117.2264 2020-02-02    43 confirmed
    ## 492         Beijing Mainland China 40.18240 116.4142 2020-02-02    23 confirmed
    ## 493       Chongqing Mainland China 30.05720 107.8740 2020-02-02    53 confirmed
    ## 494          Fujian Mainland China 26.07890 117.9874 2020-02-02    15 confirmed
    ## 495           Gansu Mainland China 36.06110 103.8343 2020-02-02    11 confirmed
    ## 496       Guangdong Mainland China 23.34170 113.4244 2020-02-02    97 confirmed
    ## 497         Guangxi Mainland China 23.82980 108.7881 2020-02-02    11 confirmed
    ## 498         Guizhou Mainland China 26.81540 106.8748 2020-02-02     9 confirmed
    ## 499          Hainan Mainland China 19.19590 109.7453 2020-02-02     2 confirmed
    ## 500           Hebei Mainland China 38.04280 114.5149 2020-02-02     8 confirmed
    ## 501    Heilongjiang Mainland China 47.86200 127.7615 2020-02-02    15 confirmed
    ## 502           Henan Mainland China 33.88202 113.6140 2020-02-02    71 confirmed
    ## 504           Hubei Mainland China 30.97560 112.2707 2020-02-02  4024 confirmed
    ## 505           Hunan Mainland China 27.61040 111.7088 2020-02-02    74 confirmed
    ## 506  Inner Mongolia Mainland China 44.09350 113.9448 2020-02-02     4 confirmed
    ## 507         Jiangsu Mainland China 32.97110 119.4550 2020-02-02    34 confirmed
    ## 508         Jiangxi Mainland China 27.61400 115.7221 2020-02-02    47 confirmed
    ## 509           Jilin Mainland China 43.66610 126.1923 2020-02-02     6 confirmed
    ## 510        Liaoning Mainland China 41.29560 122.6085 2020-02-02     6 confirmed
    ## 512         Ningxia Mainland China 37.26920 106.1655 2020-02-02     2 confirmed
    ## 513         Qinghai Mainland China 35.74520  95.9956 2020-02-02     2 confirmed
    ## 515         Shaanxi Mainland China 35.19170 108.8701 2020-02-02    15 confirmed
    ## 516        Shandong Mainland China 36.34270 118.1498 2020-02-02    24 confirmed
    ## 517        Shanghai Mainland China 31.20200 121.4491 2020-02-02    13 confirmed
    ## 518          Shanxi Mainland China 37.57770 112.2922 2020-02-02    19 confirmed
    ## 519         Sichuan Mainland China 30.61710 102.7103 2020-02-02    24 confirmed
    ## 521         Tianjin Mainland China 39.30540 117.3230 2020-02-02     7 confirmed
    ## 522        Xinjiang Mainland China 41.11290  85.2401 2020-02-02     3 confirmed
    ## 523          Yunnan Mainland China 24.97400 101.4870 2020-02-02    12 confirmed
    ## 524        Zhejiang Mainland China 29.18320 120.0934 2020-02-02    62 confirmed
    ## 526       Chongqing Mainland China 30.05720 107.8740 2020-02-02     1     death
    ## 527           Hubei Mainland China 30.97560 112.2707 2020-02-02   101     death
    ## 528           Anhui Mainland China 31.82570 117.2264 2020-02-02     2 recovered
    ## 529       Chongqing Mainland China 30.05720 107.8740 2020-02-02     4 recovered
    ## 530           Gansu Mainland China 36.06110 103.8343 2020-02-02     3 recovered
    ## 531       Guangdong Mainland China 23.34170 113.4244 2020-02-02     1 recovered
    ## 532          Hainan Mainland China 19.19590 109.7453 2020-02-02     3 recovered
    ## 533           Hebei Mainland China 38.04280 114.5149 2020-02-02     3 recovered
    ## 534           Henan Mainland China 33.88202 113.6140 2020-02-02     7 recovered
    ## 535           Hubei Mainland China 30.97560 112.2707 2020-02-02   127 recovered
    ## 536           Hunan Mainland China 27.61040 111.7088 2020-02-02     8 recovered
    ## 537         Jiangsu Mainland China 32.97110 119.4550 2020-02-02     1 recovered
    ## 538         Jiangxi Mainland China 27.61400 115.7221 2020-02-02     3 recovered
    ## 539        Shandong Mainland China 36.34270 118.1498 2020-02-02     3 recovered
    ## 540          Shanxi Mainland China 37.57770 112.2922 2020-02-02     2 recovered
    ## 541         Sichuan Mainland China 30.61710 102.7103 2020-02-02     8 recovered
    ## 542         Tianjin Mainland China 39.30540 117.3230 2020-02-02     1 recovered
    ## 543          Yunnan Mainland China 24.97400 101.4870 2020-02-02     1 recovered
    ## 544        Zhejiang Mainland China 29.18320 120.0934 2020-02-02    11 recovered
    ## 548           Anhui Mainland China 31.82570 117.2264 2020-02-03    68 confirmed
    ## 549         Beijing Mainland China 40.18240 116.4142 2020-02-03    21 confirmed
    ## 550       Chongqing Mainland China 30.05720 107.8740 2020-02-03    37 confirmed
    ## 551          Fujian Mainland China 26.07890 117.9874 2020-02-03    20 confirmed
    ## 552           Gansu Mainland China 36.06110 103.8343 2020-02-03     4 confirmed
    ## 553       Guangdong Mainland China 23.34170 113.4244 2020-02-03    93 confirmed
    ## 554         Guangxi Mainland China 23.82980 108.7881 2020-02-03    16 confirmed
    ## 555         Guizhou Mainland China 26.81540 106.8748 2020-02-03     8 confirmed
    ## 556          Hainan Mainland China 19.19590 109.7453 2020-02-03     8 confirmed
    ## 557           Hebei Mainland China 38.04280 114.5149 2020-02-03     9 confirmed
    ## 558    Heilongjiang Mainland China 47.86200 127.7615 2020-02-03    26 confirmed
    ## 559           Henan Mainland China 33.88202 113.6140 2020-02-03    73 confirmed
    ## 560           Hubei Mainland China 30.97560 112.2707 2020-02-03  2345 confirmed
    ## 561           Hunan Mainland China 27.61040 111.7088 2020-02-03    58 confirmed
    ## 562  Inner Mongolia Mainland China 44.09350 113.9448 2020-02-03     7 confirmed
    ## 563         Jiangsu Mainland China 32.97110 119.4550 2020-02-03    35 confirmed
    ## 564         Jiangxi Mainland China 27.61400 115.7221 2020-02-03    58 confirmed
    ## 565           Jilin Mainland China 43.66610 126.1923 2020-02-03     8 confirmed
    ## 566        Liaoning Mainland China 41.29560 122.6085 2020-02-03     4 confirmed
    ## 567         Ningxia Mainland China 37.26920 106.1655 2020-02-03     3 confirmed
    ## 568         Qinghai Mainland China 35.74520  95.9956 2020-02-03     2 confirmed
    ## 571         Shaanxi Mainland China 35.19170 108.8701 2020-02-03    12 confirmed
    ## 572        Shandong Mainland China 36.34270 118.1498 2020-02-03    29 confirmed
    ## 573        Shanghai Mainland China 31.20200 121.4491 2020-02-03    21 confirmed
    ## 574          Shanxi Mainland China 37.57770 112.2922 2020-02-03     8 confirmed
    ## 575         Sichuan Mainland China 30.61710 102.7103 2020-02-03    23 confirmed
    ## 576         Tianjin Mainland China 39.30540 117.3230 2020-02-03    12 confirmed
    ## 577        Xinjiang Mainland China 41.11290  85.2401 2020-02-03     3 confirmed
    ## 578          Yunnan Mainland China 24.97400 101.4870 2020-02-03    12 confirmed
    ## 579        Zhejiang Mainland China 29.18320 120.0934 2020-02-03    63 confirmed
    ## 580           Hubei Mainland China 30.97560 112.2707 2020-02-03    64     death
    ## 581           Anhui Mainland China 31.82570 117.2264 2020-02-03     7 recovered
    ## 582         Beijing Mainland China 40.18240 116.4142 2020-02-03     3 recovered
    ## 583       Chongqing Mainland China 30.05720 107.8740 2020-02-03     2 recovered
    ## 584          Fujian Mainland China 26.07890 117.9874 2020-02-03     1 recovered
    ## 585       Guangdong Mainland China 23.34170 113.4244 2020-02-03     6 recovered
    ## 586         Guangxi Mainland China 23.82980 108.7881 2020-02-03     5 recovered
    ## 587           Henan Mainland China 33.88202 113.6140 2020-02-03     6 recovered
    ## 588           Hubei Mainland China 30.97560 112.2707 2020-02-03    91 recovered
    ## 589           Hunan Mainland China 27.61040 111.7088 2020-02-03     6 recovered
    ## 590         Jiangsu Mainland China 32.97110 119.4550 2020-02-03     1 recovered
    ## 591         Jiangxi Mainland China 27.61400 115.7221 2020-02-03     6 recovered
    ## 592         Ningxia Mainland China 37.26920 106.1655 2020-02-03     1 recovered
    ## 593        Shandong Mainland China 36.34270 118.1498 2020-02-03     1 recovered
    ## 594          Shanxi Mainland China 37.57770 112.2922 2020-02-03    -1 recovered
    ## 595         Sichuan Mainland China 30.61710 102.7103 2020-02-03     3 recovered
    ## 596          Yunnan Mainland China 24.97400 101.4870 2020-02-03     2 recovered
    ## 597        Zhejiang Mainland China 29.18320 120.0934 2020-02-03    11 recovered
    ## 604           Anhui Mainland China 31.82570 117.2264 2020-02-04    72 confirmed
    ## 605         Beijing Mainland China 40.18240 116.4142 2020-02-04    16 confirmed
    ## 606       Chongqing Mainland China 30.05720 107.8740 2020-02-04    29 confirmed
    ## 607          Fujian Mainland China 26.07890 117.9874 2020-02-04    15 confirmed
    ## 608           Gansu Mainland China 36.06110 103.8343 2020-02-04     2 confirmed
    ## 609       Guangdong Mainland China 23.34170 113.4244 2020-02-04    88 confirmed
    ## 610         Guangxi Mainland China 23.82980 108.7881 2020-02-04    12 confirmed
    ## 611         Guizhou Mainland China 26.81540 106.8748 2020-02-04    12 confirmed
    ## 612          Hainan Mainland China 19.19590 109.7453 2020-02-04     8 confirmed
    ## 613           Hebei Mainland China 38.04280 114.5149 2020-02-04    13 confirmed
    ## 614    Heilongjiang Mainland China 47.86200 127.7615 2020-02-04    34 confirmed
    ## 615           Henan Mainland China 33.88202 113.6140 2020-02-04   109 confirmed
    ## 617           Hubei Mainland China 30.97560 112.2707 2020-02-04  3156 confirmed
    ## 618           Hunan Mainland China 27.61040 111.7088 2020-02-04    72 confirmed
    ## 619  Inner Mongolia Mainland China 44.09350 113.9448 2020-02-04     1 confirmed
    ## 620         Jiangsu Mainland China 32.97110 119.4550 2020-02-04    37 confirmed
    ## 621         Jiangxi Mainland China 27.61400 115.7221 2020-02-04    85 confirmed
    ## 622           Jilin Mainland China 43.66610 126.1923 2020-02-04    11 confirmed
    ## 623        Liaoning Mainland China 41.29560 122.6085 2020-02-04     7 confirmed
    ## 625         Ningxia Mainland China 37.26920 106.1655 2020-02-04     3 confirmed
    ## 626         Qinghai Mainland China 35.74520  95.9956 2020-02-04     2 confirmed
    ## 628         Shaanxi Mainland China 35.19170 108.8701 2020-02-04    14 confirmed
    ## 629        Shandong Mainland China 36.34270 118.1498 2020-02-04    16 confirmed
    ## 630        Shanghai Mainland China 31.20200 121.4491 2020-02-04    16 confirmed
    ## 631          Shanxi Mainland China 37.57770 112.2922 2020-02-04     7 confirmed
    ## 632         Sichuan Mainland China 30.61710 102.7103 2020-02-04    28 confirmed
    ## 634         Tianjin Mainland China 39.30540 117.3230 2020-02-04     7 confirmed
    ## 635        Xinjiang Mainland China 41.11290  85.2401 2020-02-04     5 confirmed
    ## 636          Yunnan Mainland China 24.97400 101.4870 2020-02-04     5 confirmed
    ## 637        Zhejiang Mainland China 29.18320 120.0934 2020-02-04   105 confirmed
    ## 639           Hubei Mainland China 30.97560 112.2707 2020-02-04    65     death
    ## 640           Anhui Mainland China 31.82570 117.2264 2020-02-04     6 recovered
    ## 641         Beijing Mainland China 40.18240 116.4142 2020-02-04    11 recovered
    ## 642          Fujian Mainland China 26.07890 117.9874 2020-02-04     2 recovered
    ## 643           Gansu Mainland China 36.06110 103.8343 2020-02-04     1 recovered
    ## 644       Guangdong Mainland China 23.34170 113.4244 2020-02-04     9 recovered
    ## 645         Guangxi Mainland China 23.82980 108.7881 2020-02-04     3 recovered
    ## 646          Hainan Mainland China 19.19590 109.7453 2020-02-04     1 recovered
    ## 647           Hebei Mainland China 38.04280 114.5149 2020-02-04     1 recovered
    ## 648    Heilongjiang Mainland China 47.86200 127.7615 2020-02-04     2 recovered
    ## 649           Henan Mainland China 33.88202 113.6140 2020-02-04    11 recovered
    ## 650           Hubei Mainland China 30.97560 112.2707 2020-02-04   136 recovered
    ## 651           Hunan Mainland China 27.61040 111.7088 2020-02-04     9 recovered
    ## 652         Jiangsu Mainland China 32.97110 119.4550 2020-02-04     4 recovered
    ## 653         Jiangxi Mainland China 27.61400 115.7221 2020-02-04     2 recovered
    ## 654        Liaoning Mainland China 41.29560 122.6085 2020-02-04     1 recovered
    ## 655         Shaanxi Mainland China 35.19170 108.8701 2020-02-04     2 recovered
    ## 656        Shandong Mainland China 36.34270 118.1498 2020-02-04     4 recovered
    ## 657        Shanghai Mainland China 31.20200 121.4491 2020-02-04     2 recovered
    ## 658          Shanxi Mainland China 37.57770 112.2922 2020-02-04     2 recovered
    ## 659         Tianjin Mainland China 39.30540 117.3230 2020-02-04     1 recovered
    ## 660        Zhejiang Mainland China 29.18320 120.0934 2020-02-04    19 recovered
    ## 664           Anhui Mainland China 31.82570 117.2264 2020-02-05    50 confirmed
    ## 665         Beijing Mainland China 40.18240 116.4142 2020-02-05    25 confirmed
    ## 667       Chongqing Mainland China 30.05720 107.8740 2020-02-05    23 confirmed
    ## 668          Fujian Mainland China 26.07890 117.9874 2020-02-05    11 confirmed
    ## 669           Gansu Mainland China 36.06110 103.8343 2020-02-05     5 confirmed
    ## 670       Guangdong Mainland China 23.34170 113.4244 2020-02-05    82 confirmed
    ## 671         Guangxi Mainland China 23.82980 108.7881 2020-02-05    11 confirmed
    ## 672         Guizhou Mainland China 26.81540 106.8748 2020-02-05     6 confirmed
    ## 673          Hainan Mainland China 19.19590 109.7453 2020-02-05    19 confirmed
    ## 674           Hebei Mainland China 38.04280 114.5149 2020-02-05     9 confirmed
    ## 675    Heilongjiang Mainland China 47.86200 127.7615 2020-02-05    35 confirmed
    ## 676           Henan Mainland China 33.88202 113.6140 2020-02-05    89 confirmed
    ## 678           Hubei Mainland China 30.97560 112.2707 2020-02-05  2987 confirmed
    ## 679           Hunan Mainland China 27.61040 111.7088 2020-02-05    68 confirmed
    ## 680  Inner Mongolia Mainland China 44.09350 113.9448 2020-02-05     7 confirmed
    ## 681         Jiangsu Mainland China 32.97110 119.4550 2020-02-05    33 confirmed
    ## 682         Jiangxi Mainland China 27.61400 115.7221 2020-02-05    72 confirmed
    ## 683           Jilin Mainland China 43.66610 126.1923 2020-02-05    12 confirmed
    ## 684        Liaoning Mainland China 41.29560 122.6085 2020-02-05     8 confirmed
    ## 686         Qinghai Mainland China 35.74520  95.9956 2020-02-05     2 confirmed
    ## 687         Shaanxi Mainland China 35.19170 108.8701 2020-02-05    23 confirmed
    ## 688        Shandong Mainland China 36.34270 118.1498 2020-02-05    32 confirmed
    ## 689        Shanghai Mainland China 31.20200 121.4491 2020-02-05    24 confirmed
    ## 690         Sichuan Mainland China 30.61710 102.7103 2020-02-05    19 confirmed
    ## 691         Tianjin Mainland China 39.30540 117.3230 2020-02-05     2 confirmed
    ## 692        Xinjiang Mainland China 41.11290  85.2401 2020-02-05     3 confirmed
    ## 693          Yunnan Mainland China 24.97400 101.4870 2020-02-05     6 confirmed
    ## 694        Zhejiang Mainland China 29.18320 120.0934 2020-02-05    66 confirmed
    ## 695         Guizhou Mainland China 26.81540 106.8748 2020-02-05     1     death
    ## 696           Hubei Mainland China 30.97560 112.2707 2020-02-05    70     death
    ## 697         Tianjin Mainland China 39.30540 117.3230 2020-02-05     1     death
    ## 698           Anhui Mainland China 31.82570 117.2264 2020-02-05     3 recovered
    ## 699         Beijing Mainland China 40.18240 116.4142 2020-02-05     1 recovered
    ## 700       Chongqing Mainland China 30.05720 107.8740 2020-02-05     6 recovered
    ## 701          Fujian Mainland China 26.07890 117.9874 2020-02-05     8 recovered
    ## 702           Gansu Mainland China 36.06110 103.8343 2020-02-05     2 recovered
    ## 703       Guangdong Mainland China 23.34170 113.4244 2020-02-05    19 recovered
    ## 704         Guangxi Mainland China 23.82980 108.7881 2020-02-05     3 recovered
    ## 705         Guizhou Mainland China 26.81540 106.8748 2020-02-05     7 recovered
    ## 706           Hebei Mainland China 38.04280 114.5149 2020-02-05     2 recovered
    ## 707    Heilongjiang Mainland China 47.86200 127.7615 2020-02-05     3 recovered
    ## 708           Henan Mainland China 33.88202 113.6140 2020-02-05    20 recovered
    ## 709           Hubei Mainland China 30.97560 112.2707 2020-02-05   111 recovered
    ## 710           Hunan Mainland China 27.61040 111.7088 2020-02-05    23 recovered
    ## 711  Inner Mongolia Mainland China 44.09350 113.9448 2020-02-05     2 recovered
    ## 712         Jiangsu Mainland China 32.97110 119.4550 2020-02-05    11 recovered
    ## 713         Jiangxi Mainland China 27.61400 115.7221 2020-02-05     7 recovered
    ## 714           Jilin Mainland China 43.66610 126.1923 2020-02-05     1 recovered
    ## 715        Liaoning Mainland China 41.29560 122.6085 2020-02-05     2 recovered
    ## 716         Qinghai Mainland China 35.74520  95.9956 2020-02-05     3 recovered
    ## 717         Shaanxi Mainland China 35.19170 108.8701 2020-02-05     4 recovered
    ## 718        Shandong Mainland China 36.34270 118.1498 2020-02-05     4 recovered
    ## 719        Shanghai Mainland China 31.20200 121.4491 2020-02-05     3 recovered
    ## 720          Shanxi Mainland China 37.57770 112.2922 2020-02-05     1 recovered
    ## 721         Sichuan Mainland China 30.61710 102.7103 2020-02-05    10 recovered
    ## 722        Zhejiang Mainland China 29.18320 120.0934 2020-02-05    16 recovered
    ## 726           Anhui Mainland China 31.82570 117.2264 2020-02-06    61 confirmed
    ## 727         Beijing Mainland China 40.18240 116.4142 2020-02-06    21 confirmed
    ## 728       Chongqing Mainland China 30.05720 107.8740 2020-02-06    22 confirmed
    ## 729          Fujian Mainland China 26.07890 117.9874 2020-02-06    10 confirmed
    ## 730       Guangdong Mainland China 23.34170 113.4244 2020-02-06    75 confirmed
    ## 731         Guangxi Mainland China 23.82980 108.7881 2020-02-06    18 confirmed
    ## 732         Guizhou Mainland China 26.81540 106.8748 2020-02-06     7 confirmed
    ## 733          Hainan Mainland China 19.19590 109.7453 2020-02-06     7 confirmed
    ## 734           Hebei Mainland China 38.04280 114.5149 2020-02-06    22 confirmed
    ## 735    Heilongjiang Mainland China 47.86200 127.7615 2020-02-06    37 confirmed
    ## 736           Henan Mainland China 33.88202 113.6140 2020-02-06    87 confirmed
    ## 738           Hubei Mainland China 30.97560 112.2707 2020-02-06  2447 confirmed
    ## 739           Hunan Mainland China 27.61040 111.7088 2020-02-06    50 confirmed
    ## 740  Inner Mongolia Mainland China 44.09350 113.9448 2020-02-06     4 confirmed
    ## 741         Jiangsu Mainland China 32.97110 119.4550 2020-02-06    32 confirmed
    ## 742         Jiangxi Mainland China 27.61400 115.7221 2020-02-06    52 confirmed
    ## 743           Jilin Mainland China 43.66610 126.1923 2020-02-06     5 confirmed
    ## 744        Liaoning Mainland China 41.29560 122.6085 2020-02-06     5 confirmed
    ## 745         Ningxia Mainland China 37.26920 106.1655 2020-02-06     6 confirmed
    ## 746         Qinghai Mainland China 35.74520  95.9956 2020-02-06     1 confirmed
    ## 748         Shaanxi Mainland China 35.19170 108.8701 2020-02-06     8 confirmed
    ## 749        Shandong Mainland China 36.34270 118.1498 2020-02-06    40 confirmed
    ## 750        Shanghai Mainland China 31.20200 121.4491 2020-02-06    14 confirmed
    ## 751          Shanxi Mainland China 37.57770 112.2922 2020-02-06    15 confirmed
    ## 752         Sichuan Mainland China 30.61710 102.7103 2020-02-06    20 confirmed
    ## 754         Tianjin Mainland China 39.30540 117.3230 2020-02-06    10 confirmed
    ## 755        Xinjiang Mainland China 41.11290  85.2401 2020-02-06     4 confirmed
    ## 756          Yunnan Mainland China 24.97400 101.4870 2020-02-06     5 confirmed
    ## 757        Zhejiang Mainland China 29.18320 120.0934 2020-02-06    59 confirmed
    ## 758    Heilongjiang Mainland China 47.86200 127.7615 2020-02-06     1     death
    ## 759           Hubei Mainland China 30.97560 112.2707 2020-02-06    69     death
    ## 760           Anhui Mainland China 31.82570 117.2264 2020-02-06    11 recovered
    ## 761         Beijing Mainland China 40.18240 116.4142 2020-02-06     7 recovered
    ## 762       Chongqing Mainland China 30.05720 107.8740 2020-02-06     9 recovered
    ## 763          Fujian Mainland China 26.07890 117.9874 2020-02-06     3 recovered
    ## 764       Guangdong Mainland China 23.34170 113.4244 2020-02-06    20 recovered
    ## 765         Guangxi Mainland China 23.82980 108.7881 2020-02-06     1 recovered
    ## 766         Guizhou Mainland China 26.81540 106.8748 2020-02-06    -3 recovered
    ## 767          Hainan Mainland China 19.19590 109.7453 2020-02-06     3 recovered
    ## 768           Hebei Mainland China 38.04280 114.5149 2020-02-06     7 recovered
    ## 769    Heilongjiang Mainland China 47.86200 127.7615 2020-02-06     1 recovered
    ## 770           Henan Mainland China 33.88202 113.6140 2020-02-06     9 recovered
    ## 771           Hubei Mainland China 30.97560 112.2707 2020-02-06   184 recovered
    ## 772           Hunan Mainland China 27.61040 111.7088 2020-02-06    27 recovered
    ## 773  Inner Mongolia Mainland China 44.09350 113.9448 2020-02-06     1 recovered
    ## 774         Jiangsu Mainland China 32.97110 119.4550 2020-02-06    11 recovered
    ## 775         Jiangxi Mainland China 27.61400 115.7221 2020-02-06    10 recovered
    ## 776           Jilin Mainland China 43.66610 126.1923 2020-02-06     2 recovered
    ## 777        Liaoning Mainland China 41.29560 122.6085 2020-02-06     1 recovered
    ## 779         Shaanxi Mainland China 35.19170 108.8701 2020-02-06     3 recovered
    ## 780        Shandong Mainland China 36.34270 118.1498 2020-02-06    12 recovered
    ## 781        Shanghai Mainland China 31.20200 121.4491 2020-02-06    10 recovered
    ## 782          Shanxi Mainland China 37.57770 112.2922 2020-02-06     7 recovered
    ## 783         Sichuan Mainland China 30.61710 102.7103 2020-02-06     7 recovered
    ## 785          Yunnan Mainland China 24.97400 101.4870 2020-02-06     2 recovered
    ## 786        Zhejiang Mainland China 29.18320 120.0934 2020-02-06    16 recovered
    ## 794           Anhui Mainland China 31.82570 117.2264 2020-02-07    74 confirmed
    ## 795         Beijing Mainland China 40.18240 116.4142 2020-02-07    23 confirmed
    ## 797       Chongqing Mainland China 30.05720 107.8740 2020-02-07    15 confirmed
    ## 799          Fujian Mainland China 26.07890 117.9874 2020-02-07     9 confirmed
    ## 800           Gansu Mainland China 36.06110 103.8343 2020-02-07     5 confirmed
    ## 801       Guangdong Mainland China 23.34170 113.4244 2020-02-07    64 confirmed
    ## 802         Guangxi Mainland China 23.82980 108.7881 2020-02-07     4 confirmed
    ## 803         Guizhou Mainland China 26.81540 106.8748 2020-02-07    10 confirmed
    ## 804          Hainan Mainland China 19.19590 109.7453 2020-02-07    11 confirmed
    ## 805           Hebei Mainland China 38.04280 114.5149 2020-02-07    15 confirmed
    ## 806    Heilongjiang Mainland China 47.86200 127.7615 2020-02-07    50 confirmed
    ## 807           Henan Mainland China 33.88202 113.6140 2020-02-07    63 confirmed
    ## 809           Hubei Mainland China 30.97560 112.2707 2020-02-07  2841 confirmed
    ## 810           Hunan Mainland China 27.61040 111.7088 2020-02-07    61 confirmed
    ## 811  Inner Mongolia Mainland China 44.09350 113.9448 2020-02-07     4 confirmed
    ## 812         Jiangsu Mainland China 32.97110 119.4550 2020-02-07    35 confirmed
    ## 813         Jiangxi Mainland China 27.61400 115.7221 2020-02-07    61 confirmed
    ## 814           Jilin Mainland China 43.66610 126.1923 2020-02-07     6 confirmed
    ## 815        Liaoning Mainland China 41.29560 122.6085 2020-02-07     5 confirmed
    ## 816         Ningxia Mainland China 37.26920 106.1655 2020-02-07     3 confirmed
    ## 818         Shaanxi Mainland China 35.19170 108.8701 2020-02-07    11 confirmed
    ## 819        Shandong Mainland China 36.34270 118.1498 2020-02-07    39 confirmed
    ## 820        Shanghai Mainland China 31.20200 121.4491 2020-02-07    20 confirmed
    ## 821          Shanxi Mainland China 37.57770 112.2922 2020-02-07     8 confirmed
    ## 822         Sichuan Mainland China 30.61710 102.7103 2020-02-07    23 confirmed
    ## 823         Tianjin Mainland China 39.30540 117.3230 2020-02-07     2 confirmed
    ## 824        Xinjiang Mainland China 41.11290  85.2401 2020-02-07     3 confirmed
    ## 825          Yunnan Mainland China 24.97400 101.4870 2020-02-07     5 confirmed
    ## 826        Zhejiang Mainland China 29.18320 120.0934 2020-02-07    52 confirmed
    ## 827       Guangdong Mainland China 23.34170 113.4244 2020-02-07     1     death
    ## 828          Hainan Mainland China 19.19590 109.7453 2020-02-07     1     death
    ## 829           Henan Mainland China 33.88202 113.6140 2020-02-07     1     death
    ## 830           Hubei Mainland China 30.97560 112.2707 2020-02-07    81     death
    ## 831           Jilin Mainland China 43.66610 126.1923 2020-02-07     1     death
    ## 834           Anhui Mainland China 31.82570 117.2264 2020-02-07    13 recovered
    ## 835         Beijing Mainland China 40.18240 116.4142 2020-02-07     2 recovered
    ## 836       Chongqing Mainland China 30.05720 107.8740 2020-02-07     7 recovered
    ## 837          Fujian Mainland China 26.07890 117.9874 2020-02-07     6 recovered
    ## 838           Gansu Mainland China 36.06110 103.8343 2020-02-07     3 recovered
    ## 839       Guangdong Mainland China 23.34170 113.4244 2020-02-07    19 recovered
    ## 840         Guangxi Mainland China 23.82980 108.7881 2020-02-07     3 recovered
    ## 841          Hainan Mainland China 19.19590 109.7453 2020-02-07     2 recovered
    ## 842           Hebei Mainland China 38.04280 114.5149 2020-02-07     9 recovered
    ## 843    Heilongjiang Mainland China 47.86200 127.7615 2020-02-07     4 recovered
    ## 844           Henan Mainland China 33.88202 113.6140 2020-02-07    30 recovered
    ## 845           Hubei Mainland China 30.97560 112.2707 2020-02-07   298 recovered
    ## 846           Hunan Mainland China 27.61040 111.7088 2020-02-07    31 recovered
    ## 847  Inner Mongolia Mainland China 44.09350 113.9448 2020-02-07     1 recovered
    ## 848         Jiangsu Mainland China 32.97110 119.4550 2020-02-07     9 recovered
    ## 849         Jiangxi Mainland China 27.61400 115.7221 2020-02-07     8 recovered
    ## 850        Liaoning Mainland China 41.29560 122.6085 2020-02-07     2 recovered
    ## 851         Ningxia Mainland China 37.26920 106.1655 2020-02-07     4 recovered
    ## 852         Shaanxi Mainland China 35.19170 108.8701 2020-02-07     8 recovered
    ## 853        Shandong Mainland China 36.34270 118.1498 2020-02-07    10 recovered
    ## 854        Shanghai Mainland China 31.20200 121.4491 2020-02-07     5 recovered
    ## 855          Shanxi Mainland China 37.57770 112.2922 2020-02-07     3 recovered
    ## 856         Sichuan Mainland China 30.61710 102.7103 2020-02-07    11 recovered
    ## 857          Yunnan Mainland China 24.97400 101.4870 2020-02-07     5 recovered
    ## 858        Zhejiang Mainland China 29.18320 120.0934 2020-02-07    29 recovered
    ## 865           Anhui Mainland China 31.82570 117.2264 2020-02-08    68 confirmed
    ## 866         Beijing Mainland China 40.18240 116.4142 2020-02-08    18 confirmed
    ## 867       Chongqing Mainland China 30.05720 107.8740 2020-02-08     2 confirmed
    ## 868          Fujian Mainland China 26.07890 117.9874 2020-02-08    15 confirmed
    ## 869           Gansu Mainland China 36.06110 103.8343 2020-02-08    12 confirmed
    ## 870       Guangdong Mainland China 23.34170 113.4244 2020-02-08    61 confirmed
    ## 871         Guangxi Mainland China 23.82980 108.7881 2020-02-08    11 confirmed
    ## 872         Guizhou Mainland China 26.81540 106.8748 2020-02-08     8 confirmed
    ## 873          Hainan Mainland China 19.19590 109.7453 2020-02-08     7 confirmed
    ## 874           Hebei Mainland China 38.04280 114.5149 2020-02-08    23 confirmed
    ## 875    Heilongjiang Mainland China 47.86200 127.7615 2020-02-08    18 confirmed
    ## 876           Henan Mainland China 33.88202 113.6140 2020-02-08    67 confirmed
    ## 878           Hubei Mainland China 30.97560 112.2707 2020-02-08  2147 confirmed
    ## 879           Hunan Mainland China 27.61040 111.7088 2020-02-08    31 confirmed
    ## 880  Inner Mongolia Mainland China 44.09350 113.9448 2020-02-08     2 confirmed
    ## 881         Jiangsu Mainland China 32.97110 119.4550 2020-02-08    31 confirmed
    ## 882         Jiangxi Mainland China 27.61400 115.7221 2020-02-08    37 confirmed
    ## 883           Jilin Mainland China 43.66610 126.1923 2020-02-08     4 confirmed
    ## 884        Liaoning Mainland China 41.29560 122.6085 2020-02-08     6 confirmed
    ## 885         Ningxia Mainland China 37.26920 106.1655 2020-02-08     2 confirmed
    ## 886         Shaanxi Mainland China 35.19170 108.8701 2020-02-08    11 confirmed
    ## 887        Shandong Mainland China 36.34270 118.1498 2020-02-08    30 confirmed
    ## 888        Shanghai Mainland China 31.20200 121.4491 2020-02-08     9 confirmed
    ## 889          Shanxi Mainland China 37.57770 112.2922 2020-02-08    11 confirmed
    ## 890         Sichuan Mainland China 30.61710 102.7103 2020-02-08    20 confirmed
    ## 892         Tianjin Mainland China 39.30540 117.3230 2020-02-08     7 confirmed
    ## 893        Xinjiang Mainland China 41.11290  85.2401 2020-02-08     3 confirmed
    ## 894        Zhejiang Mainland China 29.18320 120.0934 2020-02-08    42 confirmed
    ## 895         Beijing Mainland China 40.18240 116.4142 2020-02-08     1     death
    ## 896           Gansu Mainland China 36.06110 103.8343 2020-02-08     1     death
    ## 897    Heilongjiang Mainland China 47.86200 127.7615 2020-02-08     2     death
    ## 898           Henan Mainland China 33.88202 113.6140 2020-02-08     1     death
    ## 899           Hubei Mainland China 30.97560 112.2707 2020-02-08    81     death
    ## 900           Hunan Mainland China 27.61040 111.7088 2020-02-08     1     death
    ## 904           Anhui Mainland China 31.82570 117.2264 2020-02-08    12 recovered
    ## 905         Beijing Mainland China 40.18240 116.4142 2020-02-08     1 recovered
    ## 906       Chongqing Mainland China 30.05720 107.8740 2020-02-08     8 recovered
    ## 907          Fujian Mainland China 26.07890 117.9874 2020-02-08     4 recovered
    ## 908           Gansu Mainland China 36.06110 103.8343 2020-02-08     3 recovered
    ## 909       Guangdong Mainland China 23.34170 113.4244 2020-02-08    24 recovered
    ## 910         Guizhou Mainland China 26.81540 106.8748 2020-02-08     1 recovered
    ## 911          Hainan Mainland China 19.19590 109.7453 2020-02-08     4 recovered
    ## 912           Hebei Mainland China 38.04280 114.5149 2020-02-08     8 recovered
    ## 913    Heilongjiang Mainland China 47.86200 127.7615 2020-02-08     1 recovered
    ## 914           Henan Mainland China 33.88202 113.6140 2020-02-08    30 recovered
    ## 915           Hubei Mainland China 30.97560 112.2707 2020-02-08   324 recovered
    ## 916           Hunan Mainland China 27.61040 111.7088 2020-02-08    44 recovered
    ## 917         Jiangsu Mainland China 32.97110 119.4550 2020-02-08     8 recovered
    ## 918         Jiangxi Mainland China 27.61400 115.7221 2020-02-08    10 recovered
    ## 919        Liaoning Mainland China 41.29560 122.6085 2020-02-08     1 recovered
    ## 920         Ningxia Mainland China 37.26920 106.1655 2020-02-08    10 recovered
    ## 921         Shaanxi Mainland China 35.19170 108.8701 2020-02-08     3 recovered
    ## 922        Shandong Mainland China 36.34270 118.1498 2020-02-08     7 recovered
    ## 923        Shanghai Mainland China 31.20200 121.4491 2020-02-08    11 recovered
    ## 924          Shanxi Mainland China 37.57770 112.2922 2020-02-08     6 recovered
    ## 925         Sichuan Mainland China 30.61710 102.7103 2020-02-08    18 recovered
    ## 926         Tianjin Mainland China 39.30540 117.3230 2020-02-08     2 recovered
    ## 927          Yunnan Mainland China 24.97400 101.4870 2020-02-08     5 recovered
    ## 928        Zhejiang Mainland China 29.18320 120.0934 2020-02-08    52 recovered
    ## 934           Anhui Mainland China 31.82570 117.2264 2020-02-09    46 confirmed
    ## 935         Beijing Mainland China 40.18240 116.4142 2020-02-09    11 confirmed
    ## 936       Chongqing Mainland China 30.05720 107.8740 2020-02-09    40 confirmed
    ## 938          Fujian Mainland China 26.07890 117.9874 2020-02-09    11 confirmed
    ## 939           Gansu Mainland China 36.06110 103.8343 2020-02-09     4 confirmed
    ## 940       Guangdong Mainland China 23.34170 113.4244 2020-02-09    36 confirmed
    ## 941         Guangxi Mainland China 23.82980 108.7881 2020-02-09    12 confirmed
    ## 942         Guizhou Mainland China 26.81540 106.8748 2020-02-09    10 confirmed
    ## 943          Hainan Mainland China 19.19590 109.7453 2020-02-09     7 confirmed
    ## 944           Hebei Mainland China 38.04280 114.5149 2020-02-09    11 confirmed
    ## 945    Heilongjiang Mainland China 47.86200 127.7615 2020-02-09    12 confirmed
    ## 946           Henan Mainland China 33.88202 113.6140 2020-02-09    52 confirmed
    ## 948           Hubei Mainland China 30.97560 112.2707 2020-02-09  2531 confirmed
    ## 949           Hunan Mainland China 27.61040 111.7088 2020-02-09    35 confirmed
    ## 950  Inner Mongolia Mainland China 44.09350 113.9448 2020-02-09     2 confirmed
    ## 951         Jiangsu Mainland China 32.97110 119.4550 2020-02-09    29 confirmed
    ## 952         Jiangxi Mainland China 27.61400 115.7221 2020-02-09    42 confirmed
    ## 953           Jilin Mainland China 43.66610 126.1923 2020-02-09     9 confirmed
    ## 954        Liaoning Mainland China 41.29560 122.6085 2020-02-09     2 confirmed
    ## 955         Shaanxi Mainland China 35.19170 108.8701 2020-02-09    13 confirmed
    ## 956        Shandong Mainland China 36.34270 118.1498 2020-02-09    28 confirmed
    ## 957        Shanghai Mainland China 31.20200 121.4491 2020-02-09     7 confirmed
    ## 958          Shanxi Mainland China 37.57770 112.2922 2020-02-09     4 confirmed
    ## 959         Sichuan Mainland China 30.61710 102.7103 2020-02-09    22 confirmed
    ## 961         Tianjin Mainland China 39.30540 117.3230 2020-02-09     3 confirmed
    ## 962        Xinjiang Mainland China 41.11290  85.2401 2020-02-09     3 confirmed
    ## 963          Yunnan Mainland China 24.97400 101.4870 2020-02-09     3 confirmed
    ## 964        Zhejiang Mainland China 29.18320 120.0934 2020-02-09    27 confirmed
    ## 965           Anhui Mainland China 31.82570 117.2264 2020-02-09     1     death
    ## 966           Gansu Mainland China 36.06110 103.8343 2020-02-09     1     death
    ## 967         Guangxi Mainland China 23.82980 108.7881 2020-02-09     1     death
    ## 968          Hainan Mainland China 19.19590 109.7453 2020-02-09     1     death
    ## 969           Hebei Mainland China 38.04280 114.5149 2020-02-09     1     death
    ## 970    Heilongjiang Mainland China 47.86200 127.7615 2020-02-09     1     death
    ## 971           Henan Mainland China 33.88202 113.6140 2020-02-09     2     death
    ## 972           Hubei Mainland China 30.97560 112.2707 2020-02-09    91     death
    ## 973        Shandong Mainland China 36.34270 118.1498 2020-02-09     1     death
    ## 975           Anhui Mainland China 31.82570 117.2264 2020-02-09    13 recovered
    ## 976         Beijing Mainland China 40.18240 116.4142 2020-02-09     3 recovered
    ## 977       Chongqing Mainland China 30.05720 107.8740 2020-02-09    12 recovered
    ## 979          Fujian Mainland China 26.07890 117.9874 2020-02-09    11 recovered
    ## 980           Gansu Mainland China 36.06110 103.8343 2020-02-09     4 recovered
    ## 981       Guangdong Mainland China 23.34170 113.4244 2020-02-09    29 recovered
    ## 982         Guangxi Mainland China 23.82980 108.7881 2020-02-09     1 recovered
    ## 983          Hainan Mainland China 19.19590 109.7453 2020-02-09     5 recovered
    ## 984           Hebei Mainland China 38.04280 114.5149 2020-02-09     4 recovered
    ## 985    Heilongjiang Mainland China 47.86200 127.7615 2020-02-09     1 recovered
    ## 986           Henan Mainland China 33.88202 113.6140 2020-02-09    37 recovered
    ## 987           Hubei Mainland China 30.97560 112.2707 2020-02-09   356 recovered
    ## 988           Hunan Mainland China 27.61040 111.7088 2020-02-09    30 recovered
    ## 989         Jiangsu Mainland China 32.97110 119.4550 2020-02-09    20 recovered
    ## 990         Jiangxi Mainland China 27.61400 115.7221 2020-02-09    18 recovered
    ## 991           Jilin Mainland China 43.66610 126.1923 2020-02-09     8 recovered
    ## 993        Liaoning Mainland China 41.29560 122.6085 2020-02-09     4 recovered
    ## 994         Ningxia Mainland China 37.26920 106.1655 2020-02-09    -2 recovered
    ## 995         Shaanxi Mainland China 35.19170 108.8701 2020-02-09     5 recovered
    ## 996        Shandong Mainland China 36.34270 118.1498 2020-02-09    19 recovered
    ## 997        Shanghai Mainland China 31.20200 121.4491 2020-02-09     3 recovered
    ## 998          Shanxi Mainland China 37.57770 112.2922 2020-02-09     4 recovered
    ## 999         Sichuan Mainland China 30.61710 102.7103 2020-02-09    11 recovered
    ## 1000         Yunnan Mainland China 24.97400 101.4870 2020-02-09     1 recovered
    ## 1001       Zhejiang Mainland China 29.18320 120.0934 2020-02-09    26 recovered
    ## 1008          Anhui Mainland China 31.82570 117.2264 2020-02-10    51 confirmed
    ## 1009        Beijing Mainland China 40.18240 116.4142 2020-02-10    11 confirmed
    ## 1010      Chongqing Mainland China 30.05720 107.8740 2020-02-10    18 confirmed
    ## 1012         Fujian Mainland China 26.07890 117.9874 2020-02-10    11 confirmed
    ## 1013      Guangdong Mainland China 23.34170 113.4244 2020-02-10    28 confirmed
    ## 1014        Guangxi Mainland China 23.82980 108.7881 2020-02-10    15 confirmed
    ## 1015        Guizhou Mainland China 26.81540 106.8748 2020-02-10    10 confirmed
    ## 1016         Hainan Mainland China 19.19590 109.7453 2020-02-10     7 confirmed
    ## 1017          Hebei Mainland China 38.04280 114.5149 2020-02-10    12 confirmed
    ## 1018   Heilongjiang Mainland China 47.86200 127.7615 2020-02-10    24 confirmed
    ## 1019          Henan Mainland China 33.88202 113.6140 2020-02-10    40 confirmed
    ## 1021          Hubei Mainland China 30.97560 112.2707 2020-02-10  2097 confirmed
    ## 1022          Hunan Mainland China 27.61040 111.7088 2020-02-10    41 confirmed
    ## 1023 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-10     4 confirmed
    ## 1024        Jiangsu Mainland China 32.97110 119.4550 2020-02-10    24 confirmed
    ## 1025        Jiangxi Mainland China 27.61400 115.7221 2020-02-10    31 confirmed
    ## 1026          Jilin Mainland China 43.66610 126.1923 2020-02-10     2 confirmed
    ## 1027       Liaoning Mainland China 41.29560 122.6085 2020-02-10     1 confirmed
    ## 1028        Ningxia Mainland China 37.26920 106.1655 2020-02-10     4 confirmed
    ## 1029        Shaanxi Mainland China 35.19170 108.8701 2020-02-10     5 confirmed
    ## 1030       Shandong Mainland China 36.34270 118.1498 2020-02-10    22 confirmed
    ## 1031       Shanghai Mainland China 31.20200 121.4491 2020-02-10     6 confirmed
    ## 1032        Sichuan Mainland China 30.61710 102.7103 2020-02-10    19 confirmed
    ## 1033        Tianjin Mainland China 39.30540 117.3230 2020-02-10     4 confirmed
    ## 1034       Xinjiang Mainland China 41.11290  85.2401 2020-02-10     4 confirmed
    ## 1035         Yunnan Mainland China 24.97400 101.4870 2020-02-10     8 confirmed
    ## 1036       Zhejiang Mainland China 29.18320 120.0934 2020-02-10    17 confirmed
    ## 1037          Anhui Mainland China 31.82570 117.2264 2020-02-10     2     death
    ## 1038   Heilongjiang Mainland China 47.86200 127.7615 2020-02-10     1     death
    ## 1039          Hubei Mainland China 30.97560 112.2707 2020-02-10   103     death
    ## 1040        Jiangxi Mainland China 27.61400 115.7221 2020-02-10     1     death
    ## 1042          Anhui Mainland China 31.82570 117.2264 2020-02-10    16 recovered
    ## 1043        Beijing Mainland China 40.18240 116.4142 2020-02-10     7 recovered
    ## 1044      Chongqing Mainland China 30.05720 107.8740 2020-02-10    15 recovered
    ## 1045         Fujian Mainland China 26.07890 117.9874 2020-02-10     4 recovered
    ## 1046          Gansu Mainland China 36.06110 103.8343 2020-02-10     1 recovered
    ## 1047      Guangdong Mainland China 23.34170 113.4244 2020-02-10    26 recovered
    ## 1048        Guangxi Mainland China 23.82980 108.7881 2020-02-10     6 recovered
    ## 1049        Guizhou Mainland China 26.81540 106.8748 2020-02-10     3 recovered
    ## 1050          Hebei Mainland China 38.04280 114.5149 2020-02-10     7 recovered
    ## 1051   Heilongjiang Mainland China 47.86200 127.7615 2020-02-10    16 recovered
    ## 1052          Henan Mainland China 33.88202 113.6140 2020-02-10    38 recovered
    ## 1053          Hubei Mainland China 30.97560 112.2707 2020-02-10   427 recovered
    ## 1054          Hunan Mainland China 27.61040 111.7088 2020-02-10    22 recovered
    ## 1055        Jiangsu Mainland China 32.97110 119.4550 2020-02-10    10 recovered
    ## 1056        Jiangxi Mainland China 27.61400 115.7221 2020-02-10    32 recovered
    ## 1057          Jilin Mainland China 43.66610 126.1923 2020-02-10     1 recovered
    ## 1058       Liaoning Mainland China 41.29560 122.6085 2020-02-10     1 recovered
    ## 1059        Shaanxi Mainland China 35.19170 108.8701 2020-02-10     5 recovered
    ## 1060       Shandong Mainland China 36.34270 118.1498 2020-02-10     3 recovered
    ## 1061       Shanghai Mainland China 31.20200 121.4491 2020-02-10     4 recovered
    ## 1062        Sichuan Mainland China 30.61710 102.7103 2020-02-10     9 recovered
    ## 1063        Tianjin Mainland China 39.30540 117.3230 2020-02-10     4 recovered
    ## 1064         Yunnan Mainland China 24.97400 101.4870 2020-02-10     1 recovered
    ## 1065       Zhejiang Mainland China 29.18320 120.0934 2020-02-10    41 recovered
    ## 1071          Anhui Mainland China 31.82570 117.2264 2020-02-11    30 confirmed
    ## 1072        Beijing Mainland China 40.18240 116.4142 2020-02-11     5 confirmed
    ## 1073      Chongqing Mainland China 30.05720 107.8740 2020-02-11    19 confirmed
    ## 1074         Fujian Mainland China 26.07890 117.9874 2020-02-11     6 confirmed
    ## 1075          Gansu Mainland China 36.06110 103.8343 2020-02-11     3 confirmed
    ## 1076      Guangdong Mainland China 23.34170 113.4244 2020-02-11    18 confirmed
    ## 1077        Guangxi Mainland China 23.82980 108.7881 2020-02-11     5 confirmed
    ## 1078        Guizhou Mainland China 26.81540 106.8748 2020-02-11    18 confirmed
    ## 1079         Hainan Mainland China 19.19590 109.7453 2020-02-11     6 confirmed
    ## 1080          Hebei Mainland China 38.04280 114.5149 2020-02-11    21 confirmed
    ## 1081   Heilongjiang Mainland China 47.86200 127.7615 2020-02-11    29 confirmed
    ## 1082          Henan Mainland China 33.88202 113.6140 2020-02-11    32 confirmed
    ## 1084          Hubei Mainland China 30.97560 112.2707 2020-02-11  1638 confirmed
    ## 1085          Hunan Mainland China 27.61040 111.7088 2020-02-11    33 confirmed
    ## 1086        Jiangsu Mainland China 32.97110 119.4550 2020-02-11    23 confirmed
    ## 1087        Jiangxi Mainland China 27.61400 115.7221 2020-02-11    33 confirmed
    ## 1088          Jilin Mainland China 43.66610 126.1923 2020-02-11     1 confirmed
    ## 1089       Liaoning Mainland China 41.29560 122.6085 2020-02-11     3 confirmed
    ## 1090        Ningxia Mainland China 37.26920 106.1655 2020-02-11     4 confirmed
    ## 1092        Shaanxi Mainland China 35.19170 108.8701 2020-02-11     6 confirmed
    ## 1093       Shandong Mainland China 36.34270 118.1498 2020-02-11    21 confirmed
    ## 1094       Shanghai Mainland China 31.20200 121.4491 2020-02-11     4 confirmed
    ## 1095         Shanxi Mainland China 37.57770 112.2922 2020-02-11     5 confirmed
    ## 1096        Sichuan Mainland China 30.61710 102.7103 2020-02-11    12 confirmed
    ## 1097        Tianjin Mainland China 39.30540 117.3230 2020-02-11    11 confirmed
    ## 1098       Xinjiang Mainland China 41.11290  85.2401 2020-02-11     6 confirmed
    ## 1099         Yunnan Mainland China 24.97400 101.4870 2020-02-11     4 confirmed
    ## 1100       Zhejiang Mainland China 29.18320 120.0934 2020-02-11    25 confirmed
    ## 1101          Anhui Mainland China 31.82570 117.2264 2020-02-11     1     death
    ## 1102        Beijing Mainland China 40.18240 116.4142 2020-02-11     1     death
    ## 1103      Chongqing Mainland China 30.05720 107.8740 2020-02-11     1     death
    ## 1104   Heilongjiang Mainland China 47.86200 127.7615 2020-02-11     1     death
    ## 1105          Henan Mainland China 33.88202 113.6140 2020-02-11     1     death
    ## 1106          Hubei Mainland China 30.97560 112.2707 2020-02-11    94     death
    ## 1107        Tianjin Mainland China 39.30540 117.3230 2020-02-11     1     death
    ## 1112          Anhui Mainland China 31.82570 117.2264 2020-02-11    17 recovered
    ## 1113        Beijing Mainland China 40.18240 116.4142 2020-02-11     4 recovered
    ## 1114      Chongqing Mainland China 30.05720 107.8740 2020-02-11    13 recovered
    ## 1115         Fujian Mainland China 26.07890 117.9874 2020-02-11     6 recovered
    ## 1116          Gansu Mainland China 36.06110 103.8343 2020-02-11     7 recovered
    ## 1117      Guangdong Mainland China 23.34170 113.4244 2020-02-11    45 recovered
    ## 1118        Guangxi Mainland China 23.82980 108.7881 2020-02-11     9 recovered
    ## 1119        Guizhou Mainland China 26.81540 106.8748 2020-02-11     7 recovered
    ## 1120         Hainan Mainland China 19.19590 109.7453 2020-02-11     1 recovered
    ## 1121          Hebei Mainland China 38.04280 114.5149 2020-02-11     7 recovered
    ## 1122   Heilongjiang Mainland China 47.86200 127.7615 2020-02-11    -2 recovered
    ## 1123          Henan Mainland China 33.88202 113.6140 2020-02-11    27 recovered
    ## 1124          Hubei Mainland China 30.97560 112.2707 2020-02-11   417 recovered
    ## 1125          Hunan Mainland China 27.61040 111.7088 2020-02-11    39 recovered
    ## 1126        Jiangsu Mainland China 32.97110 119.4550 2020-02-11    12 recovered
    ## 1127        Jiangxi Mainland China 27.61400 115.7221 2020-02-11    23 recovered
    ## 1128          Jilin Mainland China 43.66610 126.1923 2020-02-11     5 recovered
    ## 1129       Liaoning Mainland China 41.29560 122.6085 2020-02-11     6 recovered
    ## 1130        Ningxia Mainland China 37.26920 106.1655 2020-02-11     9 recovered
    ## 1131        Qinghai Mainland China 35.74520  95.9956 2020-02-11     2 recovered
    ## 1132        Shaanxi Mainland China 35.19170 108.8701 2020-02-11     2 recovered
    ## 1133       Shandong Mainland China 36.34270 118.1498 2020-02-11    14 recovered
    ## 1134       Shanghai Mainland China 31.20200 121.4491 2020-02-11     4 recovered
    ## 1135         Shanxi Mainland China 37.57770 112.2922 2020-02-11     5 recovered
    ## 1136        Sichuan Mainland China 30.61710 102.7103 2020-02-11     5 recovered
    ## 1137        Tianjin Mainland China 39.30540 117.3230 2020-02-11     2 recovered
    ## 1138       Xinjiang Mainland China 41.11290  85.2401 2020-02-11     3 recovered
    ## 1139         Yunnan Mainland China 24.97400 101.4870 2020-02-11     1 recovered
    ## 1140       Zhejiang Mainland China 29.18320 120.0934 2020-02-11    28 recovered
    ## 1144          Anhui Mainland China 31.82570 117.2264 2020-02-12    29 confirmed
    ## 1145        Beijing Mainland China 40.18240 116.4142 2020-02-12    10 confirmed
    ## 1146      Chongqing Mainland China 30.05720 107.8740 2020-02-12    13 confirmed
    ## 1148         Fujian Mainland China 26.07890 117.9874 2020-02-12     5 confirmed
    ## 1149          Gansu Mainland China 36.06110 103.8343 2020-02-12     1 confirmed
    ## 1150      Guangdong Mainland China 23.34170 113.4244 2020-02-12    42 confirmed
    ## 1151        Guangxi Mainland China 23.82980 108.7881 2020-02-12     7 confirmed
    ## 1152        Guizhou Mainland China 26.81540 106.8748 2020-02-12     6 confirmed
    ## 1153         Hainan Mainland China 19.19590 109.7453 2020-02-12    13 confirmed
    ## 1154          Hebei Mainland China 38.04280 114.5149 2020-02-12    12 confirmed
    ## 1155   Heilongjiang Mainland China 47.86200 127.7615 2020-02-12    18 confirmed
    ## 1156          Henan Mainland China 33.88202 113.6140 2020-02-12    30 confirmed
    ## 1158          Hunan Mainland China 27.61040 111.7088 2020-02-12    34 confirmed
    ## 1159 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-12     2 confirmed
    ## 1160        Jiangsu Mainland China 32.97110 119.4550 2020-02-12    28 confirmed
    ## 1161        Jiangxi Mainland China 27.61400 115.7221 2020-02-12    40 confirmed
    ## 1162          Jilin Mainland China 43.66610 126.1923 2020-02-12     2 confirmed
    ## 1163       Liaoning Mainland China 41.29560 122.6085 2020-02-12     5 confirmed
    ## 1164        Ningxia Mainland China 37.26920 106.1655 2020-02-12     5 confirmed
    ## 1165        Shaanxi Mainland China 35.19170 108.8701 2020-02-12     6 confirmed
    ## 1166       Shandong Mainland China 36.34270 118.1498 2020-02-12    10 confirmed
    ## 1167       Shanghai Mainland China 31.20200 121.4491 2020-02-12     8 confirmed
    ## 1168         Shanxi Mainland China 37.57770 112.2922 2020-02-12     2 confirmed
    ## 1169        Sichuan Mainland China 30.61710 102.7103 2020-02-12    19 confirmed
    ## 1170        Tianjin Mainland China 39.30540 117.3230 2020-02-12     6 confirmed
    ## 1171       Xinjiang Mainland China 41.11290  85.2401 2020-02-12     4 confirmed
    ## 1172         Yunnan Mainland China 24.97400 101.4870 2020-02-12     1 confirmed
    ## 1173       Zhejiang Mainland China 29.18320 120.0934 2020-02-12    14 confirmed
    ## 1174         Hainan Mainland China 19.19590 109.7453 2020-02-12     1     death
    ## 1175          Henan Mainland China 33.88202 113.6140 2020-02-12     1     death
    ## 1176          Hunan Mainland China 27.61040 111.7088 2020-02-12     1     death
    ## 1177       Liaoning Mainland China 41.29560 122.6085 2020-02-12     1     death
    ## 1178       Shandong Mainland China 36.34270 118.1498 2020-02-12     1     death
    ## 1189          Anhui Mainland China 31.82570 117.2264 2020-02-12    22 recovered
    ## 1190        Beijing Mainland China 40.18240 116.4142 2020-02-12     8 recovered
    ## 1191      Chongqing Mainland China 30.05720 107.8740 2020-02-12    23 recovered
    ## 1192         Fujian Mainland China 26.07890 117.9874 2020-02-12     8 recovered
    ## 1193          Gansu Mainland China 36.06110 103.8343 2020-02-12     7 recovered
    ## 1194      Guangdong Mainland China 23.34170 113.4244 2020-02-12    63 recovered
    ## 1195        Guangxi Mainland China 23.82980 108.7881 2020-02-12    -1 recovered
    ## 1196        Guizhou Mainland China 26.81540 106.8748 2020-02-12     1 recovered
    ## 1197         Hainan Mainland China 19.19590 109.7453 2020-02-12     7 recovered
    ## 1198          Hebei Mainland China 38.04280 114.5149 2020-02-12     6 recovered
    ## 1199   Heilongjiang Mainland China 47.86200 127.7615 2020-02-12     3 recovered
    ## 1200          Henan Mainland China 33.88202 113.6140 2020-02-12    28 recovered
    ## 1202          Hubei Mainland China 30.97560 112.2707 2020-02-12    47 recovered
    ## 1203          Hunan Mainland China 27.61040 111.7088 2020-02-12    57 recovered
    ## 1204 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-12     1 recovered
    ## 1205        Jiangsu Mainland China 32.97110 119.4550 2020-02-12    32 recovered
    ## 1206        Jiangxi Mainland China 27.61400 115.7221 2020-02-12    24 recovered
    ## 1207          Jilin Mainland China 43.66610 126.1923 2020-02-12     4 recovered
    ## 1208       Liaoning Mainland China 41.29560 122.6085 2020-02-12     1 recovered
    ## 1211        Ningxia Mainland China 37.26920 106.1655 2020-02-12     2 recovered
    ## 1212        Qinghai Mainland China 35.74520  95.9956 2020-02-12     4 recovered
    ## 1213        Shaanxi Mainland China 35.19170 108.8701 2020-02-12    11 recovered
    ## 1214       Shandong Mainland China 36.34270 118.1498 2020-02-12    12 recovered
    ## 1215       Shanghai Mainland China 31.20200 121.4491 2020-02-12     5 recovered
    ## 1216         Shanxi Mainland China 37.57770 112.2922 2020-02-12     3 recovered
    ## 1217        Sichuan Mainland China 30.61710 102.7103 2020-02-12     7 recovered
    ## 1218        Tianjin Mainland China 39.30540 117.3230 2020-02-12     1 recovered
    ## 1219          Tibet Mainland China 31.69270  88.0924 2020-02-12     1 recovered
    ## 1220         Yunnan Mainland China 24.97400 101.4870 2020-02-12     6 recovered
    ## 1221       Zhejiang Mainland China 29.18320 120.0934 2020-02-12    51 recovered
    ## 1225          Anhui Mainland China 31.82570 117.2264 2020-02-13    21 confirmed
    ## 1226        Beijing Mainland China 40.18240 116.4142 2020-02-13    14 confirmed
    ## 1227      Chongqing Mainland China 30.05720 107.8740 2020-02-13    11 confirmed
    ## 1228         Fujian Mainland China 26.07890 117.9874 2020-02-13     7 confirmed
    ## 1229          Gansu Mainland China 36.06110 103.8343 2020-02-13     3 confirmed
    ## 1230      Guangdong Mainland China 23.34170 113.4244 2020-02-13    22 confirmed
    ## 1231        Guizhou Mainland China 26.81540 106.8748 2020-02-13     2 confirmed
    ## 1232          Hebei Mainland China 38.04280 114.5149 2020-02-13    14 confirmed
    ## 1233   Heilongjiang Mainland China 47.86200 127.7615 2020-02-13    17 confirmed
    ## 1234          Henan Mainland China 33.88202 113.6140 2020-02-13    34 confirmed
    ## 1236          Hubei Mainland China 30.97560 112.2707 2020-02-13 14840 confirmed
    ## 1237          Hunan Mainland China 27.61040 111.7088 2020-02-13    22 confirmed
    ## 1238 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-13     1 confirmed
    ## 1239        Jiangsu Mainland China 32.97110 119.4550 2020-02-13    27 confirmed
    ## 1240        Jiangxi Mainland China 27.61400 115.7221 2020-02-13    28 confirmed
    ## 1241          Jilin Mainland China 43.66610 126.1923 2020-02-13     1 confirmed
    ## 1242       Liaoning Mainland China 41.29560 122.6085 2020-02-13     1 confirmed
    ## 1243        Ningxia Mainland China 37.26920 106.1655 2020-02-13     6 confirmed
    ## 1246        Shaanxi Mainland China 35.19170 108.8701 2020-02-13     4 confirmed
    ## 1247       Shandong Mainland China 36.34270 118.1498 2020-02-13    12 confirmed
    ## 1248       Shanghai Mainland China 31.20200 121.4491 2020-02-13     4 confirmed
    ## 1249        Sichuan Mainland China 30.61710 102.7103 2020-02-13    15 confirmed
    ## 1250        Tianjin Mainland China 39.30540 117.3230 2020-02-13     7 confirmed
    ## 1251       Xinjiang Mainland China 41.11290  85.2401 2020-02-13     4 confirmed
    ## 1252         Yunnan Mainland China 24.97400 101.4870 2020-02-13     2 confirmed
    ## 1253       Zhejiang Mainland China 29.18320 120.0934 2020-02-13    14 confirmed
    ## 1255          Anhui Mainland China 31.82570 117.2264 2020-02-13     1     death
    ## 1256      Chongqing Mainland China 30.05720 107.8740 2020-02-13     1     death
    ## 1257      Guangdong Mainland China 23.34170 113.4244 2020-02-13     1     death
    ## 1258        Guangxi Mainland China 23.82980 108.7881 2020-02-13     1     death
    ## 1259          Hebei Mainland China 38.04280 114.5149 2020-02-13     1     death
    ## 1260   Heilongjiang Mainland China 47.86200 127.7615 2020-02-13     1     death
    ## 1261          Henan Mainland China 33.88202 113.6140 2020-02-13     2     death
    ## 1262          Hubei Mainland China 30.97560 112.2707 2020-02-13   242     death
    ## 1263        Tianjin Mainland China 39.30540 117.3230 2020-02-13     1     death
    ## 1264       Xinjiang Mainland China 41.11290  85.2401 2020-02-13     1     death
    ## 1268          Anhui Mainland China 31.82570 117.2264 2020-02-13    30 recovered
    ## 1269        Beijing Mainland China 40.18240 116.4142 2020-02-13    13 recovered
    ## 1270      Chongqing Mainland China 30.05720 107.8740 2020-02-13    26 recovered
    ## 1271         Fujian Mainland China 26.07890 117.9874 2020-02-13     4 recovered
    ## 1272          Gansu Mainland China 36.06110 103.8343 2020-02-13     8 recovered
    ## 1273      Guangdong Mainland China 23.34170 113.4244 2020-02-13    39 recovered
    ## 1274        Guangxi Mainland China 23.82980 108.7881 2020-02-13     1 recovered
    ## 1275        Guizhou Mainland China 26.81540 106.8748 2020-02-13     9 recovered
    ## 1276         Hainan Mainland China 19.19590 109.7453 2020-02-13     3 recovered
    ## 1277          Hebei Mainland China 38.04280 114.5149 2020-02-13    14 recovered
    ## 1278   Heilongjiang Mainland China 47.86200 127.7615 2020-02-13     2 recovered
    ## 1279          Henan Mainland China 33.88202 113.6140 2020-02-13    50 recovered
    ## 1280          Hubei Mainland China 30.97560 112.2707 2020-02-13   773 recovered
    ## 1281          Hunan Mainland China 27.61040 111.7088 2020-02-13    35 recovered
    ## 1282        Jiangsu Mainland China 32.97110 119.4550 2020-02-13    14 recovered
    ## 1283        Jiangxi Mainland China 27.61400 115.7221 2020-02-13    18 recovered
    ## 1284          Jilin Mainland China 43.66610 126.1923 2020-02-13     2 recovered
    ## 1285       Liaoning Mainland China 41.29560 122.6085 2020-02-13     2 recovered
    ## 1288        Qinghai Mainland China 35.74520  95.9956 2020-02-13     2 recovered
    ## 1289        Shaanxi Mainland China 35.19170 108.8701 2020-02-13     3 recovered
    ## 1290       Shandong Mainland China 36.34270 118.1498 2020-02-13    13 recovered
    ## 1291       Shanghai Mainland China 31.20200 121.4491 2020-02-13     5 recovered
    ## 1292         Shanxi Mainland China 37.57770 112.2922 2020-02-13     3 recovered
    ## 1293        Sichuan Mainland China 30.61710 102.7103 2020-02-13    12 recovered
    ## 1294        Tianjin Mainland China 39.30540 117.3230 2020-02-13    10 recovered
    ## 1296       Xinjiang Mainland China 41.11290  85.2401 2020-02-13     3 recovered
    ## 1297         Yunnan Mainland China 24.97400 101.4870 2020-02-13     1 recovered
    ## 1298       Zhejiang Mainland China 29.18320 120.0934 2020-02-13    39 recovered
    ## 1302          Anhui Mainland China 31.82570 117.2264 2020-02-14    24 confirmed
    ## 1303        Beijing Mainland China 40.18240 116.4142 2020-02-14     6 confirmed
    ## 1304      Chongqing Mainland China 30.05720 107.8740 2020-02-14     8 confirmed
    ## 1306         Fujian Mainland China 26.07890 117.9874 2020-02-14     2 confirmed
    ## 1307      Guangdong Mainland China 23.34170 113.4244 2020-02-14    20 confirmed
    ## 1308        Guangxi Mainland China 23.82980 108.7881 2020-02-14     4 confirmed
    ## 1309        Guizhou Mainland China 26.81540 106.8748 2020-02-14     5 confirmed
    ## 1310         Hainan Mainland China 19.19590 109.7453 2020-02-14     2 confirmed
    ## 1311          Hebei Mainland China 38.04280 114.5149 2020-02-14    18 confirmed
    ## 1312   Heilongjiang Mainland China 47.86200 127.7615 2020-02-14    24 confirmed
    ## 1313          Henan Mainland China 33.88202 113.6140 2020-02-14    15 confirmed
    ## 1315          Hubei Mainland China 30.97560 112.2707 2020-02-14  6200 confirmed
    ## 1316          Hunan Mainland China 27.61040 111.7088 2020-02-14    20 confirmed
    ## 1317 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-14     4 confirmed
    ## 1318        Jiangsu Mainland China 32.97110 119.4550 2020-02-14    23 confirmed
    ## 1319        Jiangxi Mainland China 27.61400 115.7221 2020-02-14    28 confirmed
    ## 1320          Jilin Mainland China 43.66610 126.1923 2020-02-14     2 confirmed
    ## 1321       Liaoning Mainland China 41.29560 122.6085 2020-02-14     2 confirmed
    ## 1322        Ningxia Mainland China 37.26920 106.1655 2020-02-14     3 confirmed
    ## 1323        Shaanxi Mainland China 35.19170 108.8701 2020-02-14     1 confirmed
    ## 1324       Shandong Mainland China 36.34270 118.1498 2020-02-14    14 confirmed
    ## 1325       Shanghai Mainland China 31.20200 121.4491 2020-02-14     3 confirmed
    ## 1326         Shanxi Mainland China 37.57770 112.2922 2020-02-14     1 confirmed
    ## 1327        Sichuan Mainland China 30.61710 102.7103 2020-02-14    12 confirmed
    ## 1328        Tianjin Mainland China 39.30540 117.3230 2020-02-14     1 confirmed
    ## 1329       Xinjiang Mainland China 41.11290  85.2401 2020-02-14     2 confirmed
    ## 1330         Yunnan Mainland China 24.97400 101.4870 2020-02-14     6 confirmed
    ## 1331       Zhejiang Mainland China 29.18320 120.0934 2020-02-14    10 confirmed
    ## 1332          Anhui Mainland China 31.82570 117.2264 2020-02-14     1     death
    ## 1333      Chongqing Mainland China 30.05720 107.8740 2020-02-14     1     death
    ## 1334   Heilongjiang Mainland China 47.86200 127.7615 2020-02-14     2     death
    ## 1335          Henan Mainland China 33.88202 113.6140 2020-02-14     1     death
    ## 1336          Hubei Mainland China 30.97560 112.2707 2020-02-14   147     death
    ## 1338          Anhui Mainland China 31.82570 117.2264 2020-02-14    36 recovered
    ## 1339        Beijing Mainland China 40.18240 116.4142 2020-02-14    11 recovered
    ## 1340      Chongqing Mainland China 30.05720 107.8740 2020-02-14    24 recovered
    ## 1341         Fujian Mainland China 26.07890 117.9874 2020-02-14     6 recovered
    ## 1342      Guangdong Mainland China 23.34170 113.4244 2020-02-14    48 recovered
    ## 1343        Guangxi Mainland China 23.82980 108.7881 2020-02-14     3 recovered
    ## 1344        Guizhou Mainland China 26.81540 106.8748 2020-02-14     1 recovered
    ## 1345         Hainan Mainland China 19.19590 109.7453 2020-02-14    13 recovered
    ## 1346          Hebei Mainland China 38.04280 114.5149 2020-02-14    19 recovered
    ## 1347   Heilongjiang Mainland China 47.86200 127.7615 2020-02-14    14 recovered
    ## 1348          Henan Mainland China 33.88202 113.6140 2020-02-14    61 recovered
    ## 1349          Hubei Mainland China 30.97560 112.2707 2020-02-14  1315 recovered
    ## 1350          Hunan Mainland China 27.61040 111.7088 2020-02-14    25 recovered
    ## 1351        Jiangsu Mainland China 32.97110 119.4550 2020-02-14    18 recovered
    ## 1352        Jiangxi Mainland China 27.61400 115.7221 2020-02-14    17 recovered
    ## 1353          Jilin Mainland China 43.66610 126.1923 2020-02-14     1 recovered
    ## 1354       Liaoning Mainland China 41.29560 122.6085 2020-02-14     7 recovered
    ## 1355        Shaanxi Mainland China 35.19170 108.8701 2020-02-14     8 recovered
    ## 1356       Shandong Mainland China 36.34270 118.1498 2020-02-14    31 recovered
    ## 1357       Shanghai Mainland China 31.20200 121.4491 2020-02-14    28 recovered
    ## 1358         Shanxi Mainland China 37.57770 112.2922 2020-02-14     2 recovered
    ## 1359        Sichuan Mainland China 30.61710 102.7103 2020-02-14    10 recovered
    ## 1361        Tianjin Mainland China 39.30540 117.3230 2020-02-14    10 recovered
    ## 1362         Yunnan Mainland China 24.97400 101.4870 2020-02-14     9 recovered
    ## 1363       Zhejiang Mainland China 29.18320 120.0934 2020-02-14    43 recovered
    ## 1368          Anhui Mainland China 31.82570 117.2264 2020-02-15    16 confirmed
    ## 1369        Beijing Mainland China 40.18240 116.4142 2020-02-15     3 confirmed
    ## 1370      Chongqing Mainland China 30.05720 107.8740 2020-02-15     7 confirmed
    ## 1372         Fujian Mainland China 26.07890 117.9874 2020-02-15     4 confirmed
    ## 1373      Guangdong Mainland China 23.34170 113.4244 2020-02-15    33 confirmed
    ## 1374        Guangxi Mainland China 23.82980 108.7881 2020-02-15     9 confirmed
    ## 1375        Guizhou Mainland China 26.81540 106.8748 2020-02-15     3 confirmed
    ## 1376         Hainan Mainland China 19.19590 109.7453 2020-02-15     3 confirmed
    ## 1377          Hebei Mainland China 38.04280 114.5149 2020-02-15     8 confirmed
    ## 1378   Heilongjiang Mainland China 47.86200 127.7615 2020-02-15     6 confirmed
    ## 1379          Henan Mainland China 33.88202 113.6140 2020-02-15    28 confirmed
    ## 1380          Hubei Mainland China 30.97560 112.2707 2020-02-15  1843 confirmed
    ## 1381          Hunan Mainland China 27.61040 111.7088 2020-02-15    13 confirmed
    ## 1382 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-15     3 confirmed
    ## 1383        Jiangsu Mainland China 32.97110 119.4550 2020-02-15    11 confirmed
    ## 1384        Jiangxi Mainland China 27.61400 115.7221 2020-02-15    13 confirmed
    ## 1385          Jilin Mainland China 43.66610 126.1923 2020-02-15     2 confirmed
    ## 1386        Ningxia Mainland China 37.26920 106.1655 2020-02-15     3 confirmed
    ## 1387        Shaanxi Mainland China 35.19170 108.8701 2020-02-15     2 confirmed
    ## 1388       Shandong Mainland China 36.34270 118.1498 2020-02-15     9 confirmed
    ## 1389       Shanghai Mainland China 31.20200 121.4491 2020-02-15     8 confirmed
    ## 1390         Shanxi Mainland China 37.57770 112.2922 2020-02-15     1 confirmed
    ## 1391        Sichuan Mainland China 30.61710 102.7103 2020-02-15     7 confirmed
    ## 1392        Tianjin Mainland China 39.30540 117.3230 2020-02-15     2 confirmed
    ## 1393       Xinjiang Mainland China 41.11290  85.2401 2020-02-15     5 confirmed
    ## 1394         Yunnan Mainland China 24.97400 101.4870 2020-02-15     6 confirmed
    ## 1395       Zhejiang Mainland China 29.18320 120.0934 2020-02-15     7 confirmed
    ## 1397        Beijing Mainland China 40.18240 116.4142 2020-02-15     1     death
    ## 1398          Henan Mainland China 33.88202 113.6140 2020-02-15     2     death
    ## 1399          Hubei Mainland China 30.97560 112.2707 2020-02-15   139     death
    ## 1407          Anhui Mainland China 31.82570 117.2264 2020-02-15    28 recovered
    ## 1408        Beijing Mainland China 40.18240 116.4142 2020-02-15    18 recovered
    ## 1409      Chongqing Mainland China 30.05720 107.8740 2020-02-15    32 recovered
    ## 1410         Fujian Mainland China 26.07890 117.9874 2020-02-15     8 recovered
    ## 1411          Gansu Mainland China 36.06110 103.8343 2020-02-15    10 recovered
    ## 1412      Guangdong Mainland China 23.34170 113.4244 2020-02-15    48 recovered
    ## 1413        Guangxi Mainland China 23.82980 108.7881 2020-02-15     8 recovered
    ## 1414        Guizhou Mainland China 26.81540 106.8748 2020-02-15    13 recovered
    ## 1415         Hainan Mainland China 19.19590 109.7453 2020-02-15    -4 recovered
    ## 1416          Hebei Mainland China 38.04280 114.5149 2020-02-15    14 recovered
    ## 1417   Heilongjiang Mainland China 47.86200 127.7615 2020-02-15    21 recovered
    ## 1418          Henan Mainland China 33.88202 113.6140 2020-02-15    34 recovered
    ## 1419          Hubei Mainland China 30.97560 112.2707 2020-02-15   849 recovered
    ## 1420          Hunan Mainland China 27.61040 111.7088 2020-02-15    61 recovered
    ## 1421 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-15     1 recovered
    ## 1422        Jiangsu Mainland China 32.97110 119.4550 2020-02-15    29 recovered
    ## 1423        Jiangxi Mainland China 27.61400 115.7221 2020-02-15    23 recovered
    ## 1424          Jilin Mainland China 43.66610 126.1923 2020-02-15     1 recovered
    ## 1425       Liaoning Mainland China 41.29560 122.6085 2020-02-15     2 recovered
    ## 1426        Ningxia Mainland China 37.26920 106.1655 2020-02-15     9 recovered
    ## 1427        Qinghai Mainland China 35.74520  95.9956 2020-02-15     2 recovered
    ## 1428        Shaanxi Mainland China 35.19170 108.8701 2020-02-15     6 recovered
    ## 1429       Shandong Mainland China 36.34270 118.1498 2020-02-15    20 recovered
    ## 1430       Shanghai Mainland China 31.20200 121.4491 2020-02-15    34 recovered
    ## 1431         Shanxi Mainland China 37.57770 112.2922 2020-02-15     8 recovered
    ## 1432        Sichuan Mainland China 30.61710 102.7103 2020-02-15     5 recovered
    ## 1433        Tianjin Mainland China 39.30540 117.3230 2020-02-15     6 recovered
    ## 1434       Xinjiang Mainland China 41.11290  85.2401 2020-02-15     4 recovered
    ## 1435         Yunnan Mainland China 24.97400 101.4870 2020-02-15     6 recovered
    ## 1436       Zhejiang Mainland China 29.18320 120.0934 2020-02-15    25 recovered
    ## 1442          Anhui Mainland China 31.82570 117.2264 2020-02-16    12 confirmed
    ## 1443        Beijing Mainland China 40.18240 116.4142 2020-02-16     5 confirmed
    ## 1444      Chongqing Mainland China 30.05720 107.8740 2020-02-16     7 confirmed
    ## 1446         Fujian Mainland China 26.07890 117.9874 2020-02-16     2 confirmed
    ## 1447      Guangdong Mainland China 23.34170 113.4244 2020-02-16    22 confirmed
    ## 1448        Guangxi Mainland China 23.82980 108.7881 2020-02-16     2 confirmed
    ## 1449        Guizhou Mainland China 26.81540 106.8748 2020-02-16     1 confirmed
    ## 1450          Hebei Mainland China 38.04280 114.5149 2020-02-16     9 confirmed
    ## 1451   Heilongjiang Mainland China 47.86200 127.7615 2020-02-16    20 confirmed
    ## 1452          Henan Mainland China 33.88202 113.6140 2020-02-16    19 confirmed
    ## 1454          Hubei Mainland China 30.97560 112.2707 2020-02-16  1933 confirmed
    ## 1455          Hunan Mainland China 27.61040 111.7088 2020-02-16     3 confirmed
    ## 1456 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-16     2 confirmed
    ## 1457        Jiangsu Mainland China 32.97110 119.4550 2020-02-16    13 confirmed
    ## 1458        Jiangxi Mainland China 27.61400 115.7221 2020-02-16    12 confirmed
    ## 1459          Jilin Mainland China 43.66610 126.1923 2020-02-16     1 confirmed
    ## 1460       Liaoning Mainland China 41.29560 122.6085 2020-02-16     2 confirmed
    ## 1461        Shaanxi Mainland China 35.19170 108.8701 2020-02-16     4 confirmed
    ## 1462       Shandong Mainland China 36.34270 118.1498 2020-02-16     5 confirmed
    ## 1463       Shanghai Mainland China 31.20200 121.4491 2020-02-16     2 confirmed
    ## 1464         Shanxi Mainland China 37.57770 112.2922 2020-02-16     1 confirmed
    ## 1465        Sichuan Mainland China 30.61710 102.7103 2020-02-16    11 confirmed
    ## 1467        Tianjin Mainland China 39.30540 117.3230 2020-02-16     2 confirmed
    ## 1468       Xinjiang Mainland China 41.11290  85.2401 2020-02-16     1 confirmed
    ## 1469         Yunnan Mainland China 24.97400 101.4870 2020-02-16     3 confirmed
    ## 1470       Zhejiang Mainland China 29.18320 120.0934 2020-02-16     5 confirmed
    ## 1471          Hubei Mainland China 30.97560 112.2707 2020-02-16   100     death
    ## 1472          Hunan Mainland China 27.61040 111.7088 2020-02-16     1     death
    ## 1473        Sichuan Mainland China 30.61710 102.7103 2020-02-16     2     death
    ## 1479          Anhui Mainland China 31.82570 117.2264 2020-02-16    34 recovered
    ## 1480        Beijing Mainland China 40.18240 116.4142 2020-02-16    10 recovered
    ## 1481      Chongqing Mainland China 30.05720 107.8740 2020-02-16    23 recovered
    ## 1482         Fujian Mainland China 26.07890 117.9874 2020-02-16    11 recovered
    ## 1483          Gansu Mainland China 36.06110 103.8343 2020-02-16     5 recovered
    ## 1484      Guangdong Mainland China 23.34170 113.4244 2020-02-16    55 recovered
    ## 1485        Guangxi Mainland China 23.82980 108.7881 2020-02-16     5 recovered
    ## 1486        Guizhou Mainland China 26.81540 106.8748 2020-02-16     5 recovered
    ## 1487         Hainan Mainland China 19.19590 109.7453 2020-02-16    13 recovered
    ## 1488          Hebei Mainland China 38.04280 114.5149 2020-02-16     4 recovered
    ## 1489   Heilongjiang Mainland China 47.86200 127.7615 2020-02-16    11 recovered
    ## 1490          Henan Mainland China 33.88202 113.6140 2020-02-16    49 recovered
    ## 1492          Hubei Mainland China 30.97560 112.2707 2020-02-16  1016 recovered
    ## 1493          Hunan Mainland China 27.61040 111.7088 2020-02-16    39 recovered
    ## 1494 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-16     1 recovered
    ## 1495        Jiangsu Mainland China 32.97110 119.4550 2020-02-16    32 recovered
    ## 1496        Jiangxi Mainland China 27.61400 115.7221 2020-02-16    30 recovered
    ## 1497          Jilin Mainland China 43.66610 126.1923 2020-02-16     4 recovered
    ## 1498       Liaoning Mainland China 41.29560 122.6085 2020-02-16     9 recovered
    ## 1500        Shaanxi Mainland China 35.19170 108.8701 2020-02-16    11 recovered
    ## 1501       Shandong Mainland China 36.34270 118.1498 2020-02-16    17 recovered
    ## 1502       Shanghai Mainland China 31.20200 121.4491 2020-02-16    16 recovered
    ## 1503         Shanxi Mainland China 37.57770 112.2922 2020-02-16     4 recovered
    ## 1504        Sichuan Mainland China 30.61710 102.7103 2020-02-16    12 recovered
    ## 1505        Tianjin Mainland China 39.30540 117.3230 2020-02-16     8 recovered
    ## 1506       Xinjiang Mainland China 41.11290  85.2401 2020-02-16     2 recovered
    ## 1507       Zhejiang Mainland China 29.18320 120.0934 2020-02-16    28 recovered
    ## 1512          Anhui Mainland China 31.82570 117.2264 2020-02-17    11 confirmed
    ## 1513        Beijing Mainland China 40.18240 116.4142 2020-02-17     1 confirmed
    ## 1515      Chongqing Mainland China 30.05720 107.8740 2020-02-17     2 confirmed
    ## 1517         Fujian Mainland China 26.07890 117.9874 2020-02-17     3 confirmed
    ## 1518          Gansu Mainland China 36.06110 103.8343 2020-02-17     1 confirmed
    ## 1519      Guangdong Mainland China 23.34170 113.4244 2020-02-17     6 confirmed
    ## 1520        Guangxi Mainland China 23.82980 108.7881 2020-02-17     1 confirmed
    ## 1521        Guizhou Mainland China 26.81540 106.8748 2020-02-17     2 confirmed
    ## 1522         Hainan Mainland China 19.19590 109.7453 2020-02-17     1 confirmed
    ## 1523          Hebei Mainland China 38.04280 114.5149 2020-02-17     1 confirmed
    ## 1524   Heilongjiang Mainland China 47.86200 127.7615 2020-02-17    12 confirmed
    ## 1525          Henan Mainland China 33.88202 113.6140 2020-02-17    15 confirmed
    ## 1527          Hubei Mainland China 30.97560 112.2707 2020-02-17  1807 confirmed
    ## 1528          Hunan Mainland China 27.61040 111.7088 2020-02-17     2 confirmed
    ## 1529 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-17     2 confirmed
    ## 1530        Jiangsu Mainland China 32.97110 119.4550 2020-02-17     9 confirmed
    ## 1531        Jiangxi Mainland China 27.61400 115.7221 2020-02-17     5 confirmed
    ## 1532        Shaanxi Mainland China 35.19170 108.8701 2020-02-17     4 confirmed
    ## 1533       Shandong Mainland China 36.34270 118.1498 2020-02-17     4 confirmed
    ## 1534       Shanghai Mainland China 31.20200 121.4491 2020-02-17     5 confirmed
    ## 1535         Shanxi Mainland China 37.57770 112.2922 2020-02-17     1 confirmed
    ## 1536        Sichuan Mainland China 30.61710 102.7103 2020-02-17    14 confirmed
    ## 1538        Tianjin Mainland China 39.30540 117.3230 2020-02-17     1 confirmed
    ## 1539       Xinjiang Mainland China 41.11290  85.2401 2020-02-17     4 confirmed
    ## 1540       Zhejiang Mainland China 29.18320 120.0934 2020-02-17     4 confirmed
    ## 1541      Guangdong Mainland China 23.34170 113.4244 2020-02-17     2     death
    ## 1542          Henan Mainland China 33.88202 113.6140 2020-02-17     3     death
    ## 1543          Hubei Mainland China 30.97560 112.2707 2020-02-17    93     death
    ## 1548          Anhui Mainland China 31.82570 117.2264 2020-02-17    25 recovered
    ## 1549        Beijing Mainland China 40.18240 116.4142 2020-02-17     6 recovered
    ## 1550      Chongqing Mainland China 30.05720 107.8740 2020-02-17    18 recovered
    ## 1551         Fujian Mainland China 26.07890 117.9874 2020-02-17     8 recovered
    ## 1552          Gansu Mainland China 36.06110 103.8343 2020-02-17     4 recovered
    ## 1553      Guangdong Mainland China 23.34170 113.4244 2020-02-17    59 recovered
    ## 1554        Guangxi Mainland China 23.82980 108.7881 2020-02-17     4 recovered
    ## 1555        Guizhou Mainland China 26.81540 106.8748 2020-02-17    11 recovered
    ## 1556         Hainan Mainland China 19.19590 109.7453 2020-02-17     7 recovered
    ## 1557          Hebei Mainland China 38.04280 114.5149 2020-02-17    17 recovered
    ## 1558   Heilongjiang Mainland China 47.86200 127.7615 2020-02-17     6 recovered
    ## 1559          Henan Mainland China 33.88202 113.6140 2020-02-17    69 recovered
    ## 1560          Hubei Mainland China 30.97560 112.2707 2020-02-17  1223 recovered
    ## 1561          Hunan Mainland China 27.61040 111.7088 2020-02-17    34 recovered
    ## 1562        Jiangsu Mainland China 32.97110 119.4550 2020-02-17    40 recovered
    ## 1563        Jiangxi Mainland China 27.61400 115.7221 2020-02-17    35 recovered
    ## 1564          Jilin Mainland China 43.66610 126.1923 2020-02-17     4 recovered
    ## 1565       Liaoning Mainland China 41.29560 122.6085 2020-02-17     3 recovered
    ## 1566        Ningxia Mainland China 37.26920 106.1655 2020-02-17     2 recovered
    ## 1567        Shaanxi Mainland China 35.19170 108.8701 2020-02-17     8 recovered
    ## 1568       Shandong Mainland China 36.34270 118.1498 2020-02-17    18 recovered
    ## 1569       Shanghai Mainland China 31.20200 121.4491 2020-02-17    21 recovered
    ## 1570         Shanxi Mainland China 37.57770 112.2922 2020-02-17     3 recovered
    ## 1571        Sichuan Mainland China 30.61710 102.7103 2020-02-17    25 recovered
    ## 1573        Tianjin Mainland China 39.30540 117.3230 2020-02-17     1 recovered
    ## 1574         Yunnan Mainland China 24.97400 101.4870 2020-02-17     5 recovered
    ## 1575       Zhejiang Mainland China 29.18320 120.0934 2020-02-17    51 recovered
    ## 1579          Anhui Mainland China 31.82570 117.2264 2020-02-18     9 confirmed
    ## 1580        Beijing Mainland China 40.18240 116.4142 2020-02-18     6 confirmed
    ## 1581      Chongqing Mainland China 30.05720 107.8740 2020-02-18     2 confirmed
    ## 1583         Fujian Mainland China 26.07890 117.9874 2020-02-18     2 confirmed
    ## 1584      Guangdong Mainland China 23.34170 113.4244 2020-02-18     6 confirmed
    ## 1585        Guangxi Mainland China 23.82980 108.7881 2020-02-18     4 confirmed
    ## 1586          Hebei Mainland China 38.04280 114.5149 2020-02-18     5 confirmed
    ## 1587   Heilongjiang Mainland China 47.86200 127.7615 2020-02-18     7 confirmed
    ## 1588          Henan Mainland China 33.88202 113.6140 2020-02-18    11 confirmed
    ## 1590          Hubei Mainland China 30.97560 112.2707 2020-02-18  1693 confirmed
    ## 1591          Hunan Mainland China 27.61040 111.7088 2020-02-18     1 confirmed
    ## 1592 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-18     1 confirmed
    ## 1593        Jiangsu Mainland China 32.97110 119.4550 2020-02-18     3 confirmed
    ## 1594        Jiangxi Mainland China 27.61400 115.7221 2020-02-18     3 confirmed
    ## 1595       Shandong Mainland China 36.34270 118.1498 2020-02-18     2 confirmed
    ## 1596         Shanxi Mainland China 37.57770 112.2922 2020-02-18     1 confirmed
    ## 1597        Sichuan Mainland China 30.61710 102.7103 2020-02-18    13 confirmed
    ## 1598        Tianjin Mainland China 39.30540 117.3230 2020-02-18     3 confirmed
    ## 1599       Xinjiang Mainland China 41.11290  85.2401 2020-02-18     1 confirmed
    ## 1600         Yunnan Mainland China 24.97400 101.4870 2020-02-18     1 confirmed
    ## 1601       Zhejiang Mainland China 29.18320 120.0934 2020-02-18     1 confirmed
    ## 1602        Guizhou Mainland China 26.81540 106.8748 2020-02-18     1     death
    ## 1603          Hebei Mainland China 38.04280 114.5149 2020-02-18     1     death
    ## 1604          Henan Mainland China 33.88202 113.6140 2020-02-18     3     death
    ## 1605          Hubei Mainland China 30.97560 112.2707 2020-02-18   132     death
    ## 1606          Hunan Mainland China 27.61040 111.7088 2020-02-18     1     death
    ## 1607       Shandong Mainland China 36.34270 118.1498 2020-02-18     1     death
    ## 1613          Anhui Mainland China 31.82570 117.2264 2020-02-18    81 recovered
    ## 1614        Beijing Mainland China 40.18240 116.4142 2020-02-18     8 recovered
    ## 1615      Chongqing Mainland China 30.05720 107.8740 2020-02-18    29 recovered
    ## 1616         Fujian Mainland China 26.07890 117.9874 2020-02-18     3 recovered
    ## 1617          Gansu Mainland China 36.06110 103.8343 2020-02-18     4 recovered
    ## 1618      Guangdong Mainland China 23.34170 113.4244 2020-02-18    41 recovered
    ## 1619        Guangxi Mainland China 23.82980 108.7881 2020-02-18    16 recovered
    ## 1620        Guizhou Mainland China 26.81540 106.8748 2020-02-18     9 recovered
    ## 1621         Hainan Mainland China 19.19590 109.7453 2020-02-18    20 recovered
    ## 1622          Hebei Mainland China 38.04280 114.5149 2020-02-18    14 recovered
    ## 1623   Heilongjiang Mainland China 47.86200 127.7615 2020-02-18    26 recovered
    ## 1624          Henan Mainland China 33.88202 113.6140 2020-02-18    13 recovered
    ## 1625          Hubei Mainland China 30.97560 112.2707 2020-02-18  1266 recovered
    ## 1626          Hunan Mainland China 27.61040 111.7088 2020-02-18    29 recovered
    ## 1627        Jiangsu Mainland China 32.97110 119.4550 2020-02-18    22 recovered
    ## 1628        Jiangxi Mainland China 27.61400 115.7221 2020-02-18    35 recovered
    ## 1629          Jilin Mainland China 43.66610 126.1923 2020-02-18     2 recovered
    ## 1630       Liaoning Mainland China 41.29560 122.6085 2020-02-18    10 recovered
    ## 1631        Ningxia Mainland China 37.26920 106.1655 2020-02-18     7 recovered
    ## 1632        Qinghai Mainland China 35.74520  95.9956 2020-02-18     2 recovered
    ## 1633        Shaanxi Mainland China 35.19170 108.8701 2020-02-18    10 recovered
    ## 1634       Shandong Mainland China 36.34270 118.1498 2020-02-18    20 recovered
    ## 1635       Shanghai Mainland China 31.20200 121.4491 2020-02-18    16 recovered
    ## 1636         Shanxi Mainland China 37.57770 112.2922 2020-02-18     8 recovered
    ## 1637        Sichuan Mainland China 30.61710 102.7103 2020-02-18    13 recovered
    ## 1638        Tianjin Mainland China 39.30540 117.3230 2020-02-18     2 recovered
    ## 1639         Yunnan Mainland China 24.97400 101.4870 2020-02-18    10 recovered
    ## 1640       Zhejiang Mainland China 29.18320 120.0934 2020-02-18    28 recovered
    ## 1644          Anhui Mainland China 31.82570 117.2264 2020-02-19     4 confirmed
    ## 1645        Beijing Mainland China 40.18240 116.4142 2020-02-19     6 confirmed
    ## 1646      Chongqing Mainland China 30.05720 107.8740 2020-02-19     5 confirmed
    ## 1648         Fujian Mainland China 26.07890 117.9874 2020-02-19     1 confirmed
    ## 1649      Guangdong Mainland China 23.34170 113.4244 2020-02-19     3 confirmed
    ## 1650        Guangxi Mainland China 23.82980 108.7881 2020-02-19     2 confirmed
    ## 1651         Hainan Mainland China 19.19590 109.7453 2020-02-19     5 confirmed
    ## 1652   Heilongjiang Mainland China 47.86200 127.7615 2020-02-19     6 confirmed
    ## 1653          Henan Mainland China 33.88202 113.6140 2020-02-19     5 confirmed
    ## 1655          Hubei Mainland China 30.97560 112.2707 2020-02-19   349 confirmed
    ## 1656          Hunan Mainland China 27.61040 111.7088 2020-02-19     1 confirmed
    ## 1657 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-19     2 confirmed
    ## 1658        Jiangsu Mainland China 32.97110 119.4550 2020-02-19     2 confirmed
    ## 1659        Jiangxi Mainland China 27.61400 115.7221 2020-02-19     1 confirmed
    ## 1660          Jilin Mainland China 43.66610 126.1923 2020-02-19     1 confirmed
    ## 1661        Ningxia Mainland China 37.26920 106.1655 2020-02-19     1 confirmed
    ## 1662        Shaanxi Mainland China 35.19170 108.8701 2020-02-19     2 confirmed
    ## 1663       Shandong Mainland China 36.34270 118.1498 2020-02-19     1 confirmed
    ## 1664        Sichuan Mainland China 30.61710 102.7103 2020-02-19     6 confirmed
    ## 1666        Tianjin Mainland China 39.30540 117.3230 2020-02-19     2 confirmed
    ## 1667       Zhejiang Mainland China 29.18320 120.0934 2020-02-19     2 confirmed
    ## 1669      Guangdong Mainland China 23.34170 113.4244 2020-02-19     1     death
    ## 1670   Heilongjiang Mainland China 47.86200 127.7615 2020-02-19     1     death
    ## 1672          Hubei Mainland China 30.97560 112.2707 2020-02-19   108     death
    ## 1673       Shanghai Mainland China 31.20200 121.4491 2020-02-19     1     death
    ## 1674         Yunnan Mainland China 24.97400 101.4870 2020-02-19     1     death
    ## 1678          Anhui Mainland China 31.82570 117.2264 2020-02-19    52 recovered
    ## 1679        Beijing Mainland China 40.18240 116.4142 2020-02-19    23 recovered
    ## 1680      Chongqing Mainland China 30.05720 107.8740 2020-02-19    20 recovered
    ## 1682         Fujian Mainland China 26.07890 117.9874 2020-02-19    19 recovered
    ## 1683          Gansu Mainland China 36.06110 103.8343 2020-02-19     3 recovered
    ## 1684      Guangdong Mainland China 23.34170 113.4244 2020-02-19    41 recovered
    ## 1685        Guangxi Mainland China 23.82980 108.7881 2020-02-19    17 recovered
    ## 1686        Guizhou Mainland China 26.81540 106.8748 2020-02-19     4 recovered
    ## 1687         Hainan Mainland China 19.19590 109.7453 2020-02-19     5 recovered
    ## 1688          Hebei Mainland China 38.04280 114.5149 2020-02-19    16 recovered
    ## 1689   Heilongjiang Mainland China 47.86200 127.7615 2020-02-19     9 recovered
    ## 1690          Henan Mainland China 33.88202 113.6140 2020-02-19    51 recovered
    ## 1692          Hubei Mainland China 30.97560 112.2707 2020-02-19  1209 recovered
    ## 1693          Hunan Mainland China 27.61040 111.7088 2020-02-19    34 recovered
    ## 1694 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-19     2 recovered
    ## 1695        Jiangsu Mainland China 32.97110 119.4550 2020-02-19    38 recovered
    ## 1696        Jiangxi Mainland China 27.61400 115.7221 2020-02-19    52 recovered
    ## 1697          Jilin Mainland China 43.66610 126.1923 2020-02-19     1 recovered
    ## 1698       Liaoning Mainland China 41.29560 122.6085 2020-02-19     2 recovered
    ## 1699        Qinghai Mainland China 35.74520  95.9956 2020-02-19     1 recovered
    ## 1700        Shaanxi Mainland China 35.19170 108.8701 2020-02-19    13 recovered
    ## 1701       Shandong Mainland China 36.34270 118.1498 2020-02-19    20 recovered
    ## 1702       Shanghai Mainland China 31.20200 121.4491 2020-02-19     9 recovered
    ## 1703         Shanxi Mainland China 37.57770 112.2922 2020-02-19     7 recovered
    ## 1704        Sichuan Mainland China 30.61710 102.7103 2020-02-19    19 recovered
    ## 1705        Tianjin Mainland China 39.30540 117.3230 2020-02-19     6 recovered
    ## 1706       Xinjiang Mainland China 41.11290  85.2401 2020-02-19     8 recovered
    ## 1707         Yunnan Mainland China 24.97400 101.4870 2020-02-19     3 recovered
    ## 1708       Zhejiang Mainland China 29.18320 120.0934 2020-02-19    69 recovered
    ## 1712          Anhui Mainland China 31.82570 117.2264 2020-02-20     1 confirmed
    ## 1713        Beijing Mainland China 40.18240 116.4142 2020-02-20     2 confirmed
    ## 1714      Chongqing Mainland China 30.05720 107.8740 2020-02-20     7 confirmed
    ## 1716      Guangdong Mainland China 23.34170 113.4244 2020-02-20     1 confirmed
    ## 1717        Guangxi Mainland China 23.82980 108.7881 2020-02-20     1 confirmed
    ## 1718          Hebei Mainland China 38.04280 114.5149 2020-02-20     1 confirmed
    ## 1719   Heilongjiang Mainland China 47.86200 127.7615 2020-02-20     6 confirmed
    ## 1720          Henan Mainland China 33.88202 113.6140 2020-02-20     3 confirmed
    ## 1722          Hubei Mainland China 30.97560 112.2707 2020-02-20   411 confirmed
    ## 1723          Hunan Mainland China 27.61040 111.7088 2020-02-20     2 confirmed
    ## 1724          Jilin Mainland China 43.66610 126.1923 2020-02-20     1 confirmed
    ## 1725        Shaanxi Mainland China 35.19170 108.8701 2020-02-20     3 confirmed
    ## 1726       Shandong Mainland China 36.34270 118.1498 2020-02-20     2 confirmed
    ## 1727       Shanghai Mainland China 31.20200 121.4491 2020-02-20     1 confirmed
    ## 1728         Shanxi Mainland China 37.57770 112.2922 2020-02-20     1 confirmed
    ## 1729        Sichuan Mainland China 30.61710 102.7103 2020-02-20     6 confirmed
    ## 1731        Tianjin Mainland China 39.30540 117.3230 2020-02-20     1 confirmed
    ## 1732         Yunnan Mainland China 24.97400 101.4870 2020-02-20     2 confirmed
    ## 1733       Zhejiang Mainland China 29.18320 120.0934 2020-02-20     1 confirmed
    ## 1735      Chongqing Mainland China 30.05720 107.8740 2020-02-20     1     death
    ## 1737         Fujian Mainland China 26.07890 117.9874 2020-02-20     1     death
    ## 1738          Hebei Mainland China 38.04280 114.5149 2020-02-20     1     death
    ## 1739          Hubei Mainland China 30.97560 112.2707 2020-02-20   115     death
    ## 1740        Shaanxi Mainland China 35.19170 108.8701 2020-02-20     1     death
    ## 1741       Shandong Mainland China 36.34270 118.1498 2020-02-20     1     death
    ## 1742         Yunnan Mainland China 24.97400 101.4870 2020-02-20     1     death
    ## 1743       Zhejiang Mainland China 29.18320 120.0934 2020-02-20     1     death
    ## 1745          Anhui Mainland China 31.82570 117.2264 2020-02-20    61 recovered
    ## 1746        Beijing Mainland China 40.18240 116.4142 2020-02-20     8 recovered
    ## 1747      Chongqing Mainland China 30.05720 107.8740 2020-02-20    25 recovered
    ## 1748         Fujian Mainland China 26.07890 117.9874 2020-02-20    14 recovered
    ## 1749          Gansu Mainland China 36.06110 103.8343 2020-02-20     6 recovered
    ## 1750      Guangdong Mainland China 23.34170 113.4244 2020-02-20    36 recovered
    ## 1751        Guangxi Mainland China 23.82980 108.7881 2020-02-20     4 recovered
    ## 1752        Guizhou Mainland China 26.81540 106.8748 2020-02-20     2 recovered
    ## 1753         Hainan Mainland China 19.19590 109.7453 2020-02-20     2 recovered
    ## 1754          Hebei Mainland China 38.04280 114.5149 2020-02-20    17 recovered
    ## 1755   Heilongjiang Mainland China 47.86200 127.7615 2020-02-20    16 recovered
    ## 1756          Henan Mainland China 33.88202 113.6140 2020-02-20    64 recovered
    ## 1758          Hubei Mainland China 30.97560 112.2707 2020-02-20  1451 recovered
    ## 1759          Hunan Mainland China 27.61040 111.7088 2020-02-20    73 recovered
    ## 1760 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-20     6 recovered
    ## 1761        Jiangsu Mainland China 32.97110 119.4550 2020-02-20    38 recovered
    ## 1762        Jiangxi Mainland China 27.61400 115.7221 2020-02-20    71 recovered
    ## 1763          Jilin Mainland China 43.66610 126.1923 2020-02-20     6 recovered
    ## 1764       Liaoning Mainland China 41.29560 122.6085 2020-02-20     4 recovered
    ## 1766        Ningxia Mainland China 37.26920 106.1655 2020-02-20     2 recovered
    ## 1767        Shaanxi Mainland China 35.19170 108.8701 2020-02-20    16 recovered
    ## 1768       Shandong Mainland China 36.34270 118.1498 2020-02-20    23 recovered
    ## 1769       Shanghai Mainland China 31.20200 121.4491 2020-02-20    13 recovered
    ## 1770         Shanxi Mainland China 37.57770 112.2922 2020-02-20     8 recovered
    ## 1771        Sichuan Mainland China 30.61710 102.7103 2020-02-20    29 recovered
    ## 1772        Tianjin Mainland China 39.30540 117.3230 2020-02-20     5 recovered
    ## 1773       Xinjiang Mainland China 41.11290  85.2401 2020-02-20     2 recovered
    ## 1774         Yunnan Mainland China 24.97400 101.4870 2020-02-20    19 recovered
    ## 1775       Zhejiang Mainland China 29.18320 120.0934 2020-02-20    29 recovered
    ## 1783          Anhui Mainland China 31.82570 117.2264 2020-02-21     1 confirmed
    ## 1784        Beijing Mainland China 40.18240 116.4142 2020-02-21     1 confirmed
    ## 1786      Chongqing Mainland China 30.05720 107.8740 2020-02-21     5 confirmed
    ## 1788      Guangdong Mainland China 23.34170 113.4244 2020-02-21     1 confirmed
    ## 1789        Guangxi Mainland China 23.82980 108.7881 2020-02-21     1 confirmed
    ## 1790          Hebei Mainland China 38.04280 114.5149 2020-02-21     1 confirmed
    ## 1791   Heilongjiang Mainland China 47.86200 127.7615 2020-02-21     3 confirmed
    ## 1792          Henan Mainland China 33.88202 113.6140 2020-02-21     2 confirmed
    ## 1793          Hubei Mainland China 30.97560 112.2707 2020-02-21   220 confirmed
    ## 1795          Hunan Mainland China 27.61040 111.7088 2020-02-21     1 confirmed
    ## 1799       Shandong Mainland China 36.34270 118.1498 2020-02-21   203 confirmed
    ## 1800        Sichuan Mainland China 30.61710 102.7103 2020-02-21     5 confirmed
    ## 1802        Tianjin Mainland China 39.30540 117.3230 2020-02-21     1 confirmed
    ## 1804       Zhejiang Mainland China 29.18320 120.0934 2020-02-21    28 confirmed
    ## 1813          Anhui Mainland China 31.82570 117.2264 2020-02-21    65 recovered
    ## 1814        Beijing Mainland China 40.18240 116.4142 2020-02-21    16 recovered
    ## 1815      Chongqing Mainland China 30.05720 107.8740 2020-02-21    17 recovered
    ## 1816         Fujian Mainland China 26.07890 117.9874 2020-02-21    23 recovered
    ## 1817          Gansu Mainland China 36.06110 103.8343 2020-02-21     5 recovered
    ## 1818      Guangdong Mainland China 23.34170 113.4244 2020-02-21    48 recovered
    ## 1819        Guangxi Mainland China 23.82980 108.7881 2020-02-21     7 recovered
    ## 1820        Guizhou Mainland China 26.81540 106.8748 2020-02-21     5 recovered
    ## 1821         Hainan Mainland China 19.19590 109.7453 2020-02-21     9 recovered
    ## 1822          Hebei Mainland China 38.04280 114.5149 2020-02-21    15 recovered
    ## 1823   Heilongjiang Mainland China 47.86200 127.7615 2020-02-21    39 recovered
    ## 1824          Henan Mainland China 33.88202 113.6140 2020-02-21    99 recovered
    ## 1826          Hubei Mainland China 30.97560 112.2707 2020-02-21    93 recovered
    ## 1827          Hunan Mainland China 27.61040 111.7088 2020-02-21    27 recovered
    ## 1828 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-21     1 recovered
    ## 1829        Jiangsu Mainland China 32.97110 119.4550 2020-02-21    17 recovered
    ## 1830        Jiangxi Mainland China 27.61400 115.7221 2020-02-21    56 recovered
    ## 1831          Jilin Mainland China 43.66610 126.1923 2020-02-21     2 recovered
    ## 1832       Liaoning Mainland China 41.29560 122.6085 2020-02-21     2 recovered
    ## 1833        Ningxia Mainland China 37.26920 106.1655 2020-02-21     4 recovered
    ## 1834        Qinghai Mainland China 35.74520  95.9956 2020-02-21     2 recovered
    ## 1838        Shaanxi Mainland China 35.19170 108.8701 2020-02-21    16 recovered
    ## 1839       Shandong Mainland China 36.34270 118.1498 2020-02-21    27 recovered
    ## 1840       Shanghai Mainland China 31.20200 121.4491 2020-02-21    12 recovered
    ## 1841         Shanxi Mainland China 37.57770 112.2922 2020-02-21     2 recovered
    ## 1842        Sichuan Mainland China 30.61710 102.7103 2020-02-21    14 recovered
    ## 1843        Tianjin Mainland China 39.30540 117.3230 2020-02-21     3 recovered
    ## 1845       Xinjiang Mainland China 41.11290  85.2401 2020-02-21     2 recovered
    ## 1846         Yunnan Mainland China 24.97400 101.4870 2020-02-21    17 recovered
    ## 1847       Zhejiang Mainland China 29.18320 120.0934 2020-02-21    46 recovered
    ## 1853          Anhui Mainland China 31.82570 117.2264 2020-02-22     1 confirmed
    ## 1854        Beijing Mainland China 40.18240 116.4142 2020-02-22     3 confirmed
    ## 1855      Chongqing Mainland China 30.05720 107.8740 2020-02-22     1 confirmed
    ## 1857      Guangdong Mainland China 23.34170 113.4244 2020-02-22     6 confirmed
    ## 1858        Guangxi Mainland China 23.82980 108.7881 2020-02-22     3 confirmed
    ## 1859          Hebei Mainland China 38.04280 114.5149 2020-02-22     1 confirmed
    ## 1860          Henan Mainland China 33.88202 113.6140 2020-02-22     3 confirmed
    ## 1862          Hubei Mainland China 30.97560 112.2707 2020-02-22  1422 confirmed
    ## 1863          Hunan Mainland China 27.61040 111.7088 2020-02-22     2 confirmed
    ## 1864       Shandong Mainland China 36.34270 118.1498 2020-02-22     1 confirmed
    ## 1865       Shanghai Mainland China 31.20200 121.4491 2020-02-22     1 confirmed
    ## 1866        Sichuan Mainland China 30.61710 102.7103 2020-02-22     1 confirmed
    ## 1867        Tianjin Mainland China 39.30540 117.3230 2020-02-22     3 confirmed
    ## 1868       Zhejiang Mainland China 29.18320 120.0934 2020-02-22     2 confirmed
    ## 1871          Hebei Mainland China 38.04280 114.5149 2020-02-22     1     death
    ## 1872          Hubei Mainland China 30.97560 112.2707 2020-02-22   202     death
    ## 1873       Shanghai Mainland China 31.20200 121.4491 2020-02-22     1     death
    ## 1874       Xinjiang Mainland China 41.11290  85.2401 2020-02-22     1     death
    ## 1876          Anhui Mainland China 31.82570 117.2264 2020-02-22    58 recovered
    ## 1877        Beijing Mainland China 40.18240 116.4142 2020-02-22     9 recovered
    ## 1878      Chongqing Mainland China 30.05720 107.8740 2020-02-22    12 recovered
    ## 1879         Fujian Mainland China 26.07890 117.9874 2020-02-22    13 recovered
    ## 1880      Guangdong Mainland China 23.34170 113.4244 2020-02-22    38 recovered
    ## 1881        Guangxi Mainland China 23.82980 108.7881 2020-02-22     7 recovered
    ## 1882        Guizhou Mainland China 26.81540 106.8748 2020-02-22    13 recovered
    ## 1883         Hainan Mainland China 19.19590 109.7453 2020-02-22     9 recovered
    ## 1884          Hebei Mainland China 38.04280 114.5149 2020-02-22    19 recovered
    ## 1885   Heilongjiang Mainland China 47.86200 127.7615 2020-02-22    29 recovered
    ## 1886          Henan Mainland China 33.88202 113.6140 2020-02-22    94 recovered
    ## 1888          Hubei Mainland China 30.97560 112.2707 2020-02-22  3418 recovered
    ## 1889          Hunan Mainland China 27.61040 111.7088 2020-02-22    31 recovered
    ## 1890 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-22     9 recovered
    ## 1891        Jiangsu Mainland China 32.97110 119.4550 2020-02-22    28 recovered
    ## 1892        Jiangxi Mainland China 27.61400 115.7221 2020-02-22    66 recovered
    ## 1893          Jilin Mainland China 43.66610 126.1923 2020-02-22     7 recovered
    ## 1894       Liaoning Mainland China 41.29560 122.6085 2020-02-22     5 recovered
    ## 1895        Shaanxi Mainland China 35.19170 108.8701 2020-02-22    15 recovered
    ## 1896       Shandong Mainland China 36.34270 118.1498 2020-02-22    21 recovered
    ## 1897       Shanghai Mainland China 31.20200 121.4491 2020-02-22    16 recovered
    ## 1898         Shanxi Mainland China 37.57770 112.2922 2020-02-22     3 recovered
    ## 1899        Sichuan Mainland China 30.61710 102.7103 2020-02-22    19 recovered
    ## 1900        Tianjin Mainland China 39.30540 117.3230 2020-02-22     3 recovered
    ## 1901       Xinjiang Mainland China 41.11290  85.2401 2020-02-22     1 recovered
    ## 1902         Yunnan Mainland China 24.97400 101.4870 2020-02-22    11 recovered
    ## 1903       Zhejiang Mainland China 29.18320 120.0934 2020-02-22    40 recovered
    ## 1909      Chongqing Mainland China 30.05720 107.8740 2020-02-23     2 confirmed
    ## 1911      Guangdong Mainland China 23.34170 113.4244 2020-02-23     3 confirmed
    ## 1912          Hebei Mainland China 38.04280 114.5149 2020-02-23     2 confirmed
    ## 1913   Heilongjiang Mainland China 47.86200 127.7615 2020-02-23     1 confirmed
    ## 1914          Henan Mainland China 33.88202 113.6140 2020-02-23     1 confirmed
    ## 1916          Hunan Mainland China 27.61040 111.7088 2020-02-23     3 confirmed
    ## 1917       Shandong Mainland China 36.34270 118.1498 2020-02-23     4 confirmed
    ## 1923      Guangdong Mainland China 23.34170 113.4244 2020-02-23     1     death
    ## 1924         Hainan Mainland China 19.19590 109.7453 2020-02-23     1     death
    ## 1929          Anhui Mainland China 31.82570 117.2264 2020-02-23    40 recovered
    ## 1930        Beijing Mainland China 40.18240 116.4142 2020-02-23    11 recovered
    ## 1931      Chongqing Mainland China 30.05720 107.8740 2020-02-23     7 recovered
    ## 1933         Fujian Mainland China 26.07890 117.9874 2020-02-23     8 recovered
    ## 1934          Gansu Mainland China 36.06110 103.8343 2020-02-23     2 recovered
    ## 1935      Guangdong Mainland China 23.34170 113.4244 2020-02-23    27 recovered
    ## 1936        Guangxi Mainland China 23.82980 108.7881 2020-02-23     2 recovered
    ## 1937        Guizhou Mainland China 26.81540 106.8748 2020-02-23    12 recovered
    ## 1938         Hainan Mainland China 19.19590 109.7453 2020-02-23     2 recovered
    ## 1939          Hebei Mainland China 38.04280 114.5149 2020-02-23    16 recovered
    ## 1940   Heilongjiang Mainland China 47.86200 127.7615 2020-02-23    18 recovered
    ## 1941          Henan Mainland China 33.88202 113.6140 2020-02-23    38 recovered
    ## 1943          Hubei Mainland China 30.97560 112.2707 2020-02-23    44 recovered
    ## 1944          Hunan Mainland China 27.61040 111.7088 2020-02-23    22 recovered
    ## 1945 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-23     1 recovered
    ## 1946        Jiangsu Mainland China 32.97110 119.4550 2020-02-23    17 recovered
    ## 1947        Jiangxi Mainland China 27.61400 115.7221 2020-02-23    58 recovered
    ## 1948          Jilin Mainland China 43.66610 126.1923 2020-02-23     2 recovered
    ## 1949       Liaoning Mainland China 41.29560 122.6085 2020-02-23     7 recovered
    ## 1950        Ningxia Mainland China 37.26920 106.1655 2020-02-23     8 recovered
    ## 1951        Shaanxi Mainland China 35.19170 108.8701 2020-02-23    14 recovered
    ## 1952       Shandong Mainland China 36.34270 118.1498 2020-02-23    19 recovered
    ## 1953       Shanghai Mainland China 31.20200 121.4491 2020-02-23    22 recovered
    ## 1954         Shanxi Mainland China 37.57770 112.2922 2020-02-23     7 recovered
    ## 1955        Sichuan Mainland China 30.61710 102.7103 2020-02-23    11 recovered
    ## 1956        Tianjin Mainland China 39.30540 117.3230 2020-02-23    16 recovered
    ## 1957       Xinjiang Mainland China 41.11290  85.2401 2020-02-23     3 recovered
    ## 1958         Yunnan Mainland China 24.97400 101.4870 2020-02-23     8 recovered
    ## 1959       Zhejiang Mainland China 29.18320 120.0934 2020-02-23    41 recovered
    ## 1970      Chongqing Mainland China 30.05720 107.8740 2020-02-24     1 confirmed
    ## 1971      Guangdong Mainland China 23.34170 113.4244 2020-02-24     3 confirmed
    ## 1972        Guangxi Mainland China 23.82980 108.7881 2020-02-24     2 confirmed
    ## 1974          Hubei Mainland China 30.97560 112.2707 2020-02-24   203 confirmed
    ## 1975          Jilin Mainland China 43.66610 126.1923 2020-02-24     2 confirmed
    ## 1978       Shandong Mainland China 36.34270 118.1498 2020-02-24     1 confirmed
    ## 1979         Shanxi Mainland China 37.57770 112.2922 2020-02-24     1 confirmed
    ## 1980        Sichuan Mainland China 30.61710 102.7103 2020-02-24     1 confirmed
    ## 1988          Hubei Mainland China 30.97560 112.2707 2020-02-24   149     death
    ## 1989       Shandong Mainland China 36.34270 118.1498 2020-02-24     1     death
    ## 1992          Anhui Mainland China 31.82570 117.2264 2020-02-24    26 recovered
    ## 1993        Beijing Mainland China 40.18240 116.4142 2020-02-24     9 recovered
    ## 1994      Chongqing Mainland China 30.05720 107.8740 2020-02-24    14 recovered
    ## 1995         Fujian Mainland China 26.07890 117.9874 2020-02-24    13 recovered
    ## 1996          Gansu Mainland China 36.06110 103.8343 2020-02-24     2 recovered
    ## 1997      Guangdong Mainland China 23.34170 113.4244 2020-02-24    31 recovered
    ## 1998        Guangxi Mainland China 23.82980 108.7881 2020-02-24     6 recovered
    ## 1999         Hainan Mainland China 19.19590 109.7453 2020-02-24    10 recovered
    ## 2000          Hebei Mainland China 38.04280 114.5149 2020-02-24    15 recovered
    ## 2001   Heilongjiang Mainland China 47.86200 127.7615 2020-02-24     5 recovered
    ## 2002          Henan Mainland China 33.88202 113.6140 2020-02-24    75 recovered
    ## 2004          Hubei Mainland China 30.97560 112.2707 2020-02-24  1405 recovered
    ## 2005          Hunan Mainland China 27.61040 111.7088 2020-02-24    17 recovered
    ## 2006 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-24     7 recovered
    ## 2007        Jiangsu Mainland China 32.97110 119.4550 2020-02-24    34 recovered
    ## 2008        Jiangxi Mainland China 27.61400 115.7221 2020-02-24    32 recovered
    ## 2009          Jilin Mainland China 43.66610 126.1923 2020-02-24     6 recovered
    ## 2010       Liaoning Mainland China 41.29560 122.6085 2020-02-24     7 recovered
    ## 2011        Ningxia Mainland China 37.26920 106.1655 2020-02-24     2 recovered
    ## 2012        Shaanxi Mainland China 35.19170 108.8701 2020-02-24    10 recovered
    ## 2013       Shandong Mainland China 36.34270 118.1498 2020-02-24    22 recovered
    ## 2014       Shanghai Mainland China 31.20200 121.4491 2020-02-24    12 recovered
    ## 2015         Shanxi Mainland China 37.57770 112.2922 2020-02-24     6 recovered
    ## 2016        Sichuan Mainland China 30.61710 102.7103 2020-02-24    15 recovered
    ## 2018        Tianjin Mainland China 39.30540 117.3230 2020-02-24     6 recovered
    ## 2019       Xinjiang Mainland China 41.11290  85.2401 2020-02-24     2 recovered
    ## 2020         Yunnan Mainland China 24.97400 101.4870 2020-02-24     9 recovered
    ## 2021       Zhejiang Mainland China 29.18320 120.0934 2020-02-24    22 recovered
    ## 2037        Beijing Mainland China 40.18240 116.4142 2020-02-25     1 confirmed
    ## 2039         Fujian Mainland China 26.07890 117.9874 2020-02-25     1 confirmed
    ## 2040      Guangdong Mainland China 23.34170 113.4244 2020-02-25     2 confirmed
    ## 2041        Guangxi Mainland China 23.82980 108.7881 2020-02-25     1 confirmed
    ## 2043          Hubei Mainland China 30.97560 112.2707 2020-02-25   499 confirmed
    ## 2044       Shandong Mainland China 36.34270 118.1498 2020-02-25     1 confirmed
    ## 2045       Shanghai Mainland China 31.20200 121.4491 2020-02-25     1 confirmed
    ## 2046        Sichuan Mainland China 30.61710 102.7103 2020-02-25     2 confirmed
    ## 2051      Guangdong Mainland China 23.34170 113.4244 2020-02-25     1     death
    ## 2052          Hubei Mainland China 30.97560 112.2707 2020-02-25    68     death
    ## 2053       Shandong Mainland China 36.34270 118.1498 2020-02-25     1     death
    ## 2059          Anhui Mainland China 31.82570 117.2264 2020-02-25    49 recovered
    ## 2060        Beijing Mainland China 40.18240 116.4142 2020-02-25    17 recovered
    ## 2061      Chongqing Mainland China 30.05720 107.8740 2020-02-25    23 recovered
    ## 2062         Fujian Mainland China 26.07890 117.9874 2020-02-25    16 recovered
    ## 2063      Guangdong Mainland China 23.34170 113.4244 2020-02-25    36 recovered
    ## 2064        Guangxi Mainland China 23.82980 108.7881 2020-02-25    22 recovered
    ## 2065        Guizhou Mainland China 26.81540 106.8748 2020-02-25     2 recovered
    ## 2066         Hainan Mainland China 19.19590 109.7453 2020-02-25     8 recovered
    ## 2067          Hebei Mainland China 38.04280 114.5149 2020-02-25    14 recovered
    ## 2068   Heilongjiang Mainland China 47.86200 127.7615 2020-02-25    16 recovered
    ## 2069          Henan Mainland China 33.88202 113.6140 2020-02-25    59 recovered
    ## 2070          Hubei Mainland China 30.97560 112.2707 2020-02-25  2223 recovered
    ## 2071          Hunan Mainland China 27.61040 111.7088 2020-02-25    37 recovered
    ## 2072 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-25     1 recovered
    ## 2073        Jiangsu Mainland China 32.97110 119.4550 2020-02-25     6 recovered
    ## 2074        Jiangxi Mainland China 27.61400 115.7221 2020-02-25    38 recovered
    ## 2075          Jilin Mainland China 43.66610 126.1923 2020-02-25     3 recovered
    ## 2076       Liaoning Mainland China 41.29560 122.6085 2020-02-25     3 recovered
    ## 2078        Ningxia Mainland China 37.26920 106.1655 2020-02-25     3 recovered
    ## 2079        Shaanxi Mainland China 35.19170 108.8701 2020-02-25    13 recovered
    ## 2080       Shandong Mainland China 36.34270 118.1498 2020-02-25    12 recovered
    ## 2081       Shanghai Mainland China 31.20200 121.4491 2020-02-25     7 recovered
    ## 2082         Shanxi Mainland China 37.57770 112.2922 2020-02-25     4 recovered
    ## 2083        Sichuan Mainland China 30.61710 102.7103 2020-02-25    13 recovered
    ## 2085        Tianjin Mainland China 39.30540 117.3230 2020-02-25     4 recovered
    ## 2086         Yunnan Mainland China 24.97400 101.4870 2020-02-25     5 recovered
    ## 2087       Zhejiang Mainland China 29.18320 120.0934 2020-02-25    26 recovered
    ## 2114          Hebei Mainland China 38.04280 114.5149 2020-02-26     1 confirmed
    ## 2116          Hubei Mainland China 30.97560 112.2707 2020-02-26   401 confirmed
    ## 2117       Shanghai Mainland China 31.20200 121.4491 2020-02-26     1 confirmed
    ## 2118        Sichuan Mainland China 30.61710 102.7103 2020-02-26     2 confirmed
    ## 2127          Hubei Mainland China 30.97560 112.2707 2020-02-26    52     death
    ## 2132          Anhui Mainland China 31.82570 117.2264 2020-02-26    32 recovered
    ## 2133        Beijing Mainland China 40.18240 116.4142 2020-02-26    20 recovered
    ## 2134      Chongqing Mainland China 30.05720 107.8740 2020-02-26    12 recovered
    ## 2136         Fujian Mainland China 26.07890 117.9874 2020-02-26    19 recovered
    ## 2137          Gansu Mainland China 36.06110 103.8343 2020-02-26     1 recovered
    ## 2138      Guangdong Mainland China 23.34170 113.4244 2020-02-26    29 recovered
    ## 2139        Guangxi Mainland China 23.82980 108.7881 2020-02-26    13 recovered
    ## 2140         Hainan Mainland China 19.19590 109.7453 2020-02-26     5 recovered
    ## 2141          Hebei Mainland China 38.04280 114.5149 2020-02-26    13 recovered
    ## 2142   Heilongjiang Mainland China 47.86200 127.7615 2020-02-26     6 recovered
    ## 2143          Henan Mainland China 33.88202 113.6140 2020-02-26    31 recovered
    ## 2145          Hubei Mainland China 30.97560 112.2707 2020-02-26  1998 recovered
    ## 2146          Hunan Mainland China 27.61040 111.7088 2020-02-26    15 recovered
    ## 2147 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-26     3 recovered
    ## 2148        Jiangsu Mainland China 32.97110 119.4550 2020-02-26    20 recovered
    ## 2149        Jiangxi Mainland China 27.61400 115.7221 2020-02-26    36 recovered
    ## 2150          Jilin Mainland China 43.66610 126.1923 2020-02-26     2 recovered
    ## 2151       Liaoning Mainland China 41.29560 122.6085 2020-02-26     5 recovered
    ## 2152        Ningxia Mainland China 37.26920 106.1655 2020-02-26     4 recovered
    ## 2153        Shaanxi Mainland China 35.19170 108.8701 2020-02-26     6 recovered
    ## 2154       Shandong Mainland China 36.34270 118.1498 2020-02-26    22 recovered
    ## 2155       Shanghai Mainland China 31.20200 121.4491 2020-02-26     4 recovered
    ## 2156         Shanxi Mainland China 37.57770 112.2922 2020-02-26     6 recovered
    ## 2157        Sichuan Mainland China 30.61710 102.7103 2020-02-26    18 recovered
    ## 2158        Tianjin Mainland China 39.30540 117.3230 2020-02-26     5 recovered
    ## 2159       Xinjiang Mainland China 41.11290  85.2401 2020-02-26     4 recovered
    ## 2160         Yunnan Mainland China 24.97400 101.4870 2020-02-26    15 recovered
    ## 2161       Zhejiang Mainland China 29.18320 120.0934 2020-02-26    59 recovered
    ## 2182        Beijing Mainland China 40.18240 116.4142 2020-02-27    10 confirmed
    ## 2184         Fujian Mainland China 26.07890 117.9874 2020-02-27     2 confirmed
    ## 2185          Hebei Mainland China 38.04280 114.5149 2020-02-27     5 confirmed
    ## 2186          Henan Mainland China 33.88202 113.6140 2020-02-27     1 confirmed
    ## 2188          Hubei Mainland China 30.97560 112.2707 2020-02-27   409 confirmed
    ## 2189          Hunan Mainland China 27.61040 111.7088 2020-02-27     1 confirmed
    ## 2190        Ningxia Mainland China 37.26920 106.1655 2020-02-27     1 confirmed
    ## 2192        Sichuan Mainland China 30.61710 102.7103 2020-02-27     3 confirmed
    ## 2193        Tianjin Mainland China 39.30540 117.3230 2020-02-27     1 confirmed
    ## 2199        Beijing Mainland China 40.18240 116.4142 2020-02-27     1     death
    ## 2200   Heilongjiang Mainland China 47.86200 127.7615 2020-02-27     1     death
    ## 2201          Henan Mainland China 33.88202 113.6140 2020-02-27     1     death
    ## 2202          Hubei Mainland China 30.97560 112.2707 2020-02-27    26     death
    ## 2206          Anhui Mainland China 31.82570 117.2264 2020-02-27    48 recovered
    ## 2207        Beijing Mainland China 40.18240 116.4142 2020-02-27    13 recovered
    ## 2209      Chongqing Mainland China 30.05720 107.8740 2020-02-27    17 recovered
    ## 2210         Fujian Mainland China 26.07890 117.9874 2020-02-27    10 recovered
    ## 2211      Guangdong Mainland China 23.34170 113.4244 2020-02-27    39 recovered
    ## 2212        Guangxi Mainland China 23.82980 108.7881 2020-02-27    14 recovered
    ## 2213        Guizhou Mainland China 26.81540 106.8748 2020-02-27     8 recovered
    ## 2214         Hainan Mainland China 19.19590 109.7453 2020-02-27     2 recovered
    ## 2215          Hebei Mainland China 38.04280 114.5149 2020-02-27    13 recovered
    ## 2216   Heilongjiang Mainland China 47.86200 127.7615 2020-02-27    21 recovered
    ## 2217          Henan Mainland China 33.88202 113.6140 2020-02-27    35 recovered
    ## 2218          Hubei Mainland China 30.97560 112.2707 2020-02-27  2414 recovered
    ## 2219          Hunan Mainland China 27.61040 111.7088 2020-02-27    21 recovered
    ## 2220 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-27     5 recovered
    ## 2221        Jiangsu Mainland China 32.97110 119.4550 2020-02-27    20 recovered
    ## 2222        Jiangxi Mainland China 27.61400 115.7221 2020-02-27    35 recovered
    ## 2223          Jilin Mainland China 43.66610 126.1923 2020-02-27     2 recovered
    ## 2224       Liaoning Mainland China 41.29560 122.6085 2020-02-27     5 recovered
    ## 2226        Ningxia Mainland China 37.26920 106.1655 2020-02-27     3 recovered
    ## 2227        Shaanxi Mainland China 35.19170 108.8701 2020-02-27     3 recovered
    ## 2228       Shandong Mainland China 36.34270 118.1498 2020-02-27    10 recovered
    ## 2229       Shanghai Mainland China 31.20200 121.4491 2020-02-27     4 recovered
    ## 2230         Shanxi Mainland China 37.57770 112.2922 2020-02-27     3 recovered
    ## 2231        Sichuan Mainland China 30.61710 102.7103 2020-02-27    14 recovered
    ## 2232        Tianjin Mainland China 39.30540 117.3230 2020-02-27     6 recovered
    ## 2233       Xinjiang Mainland China 41.11290  85.2401 2020-02-27     9 recovered
    ## 2234         Yunnan Mainland China 24.97400 101.4870 2020-02-27     6 recovered
    ## 2235       Zhejiang Mainland China 29.18320 120.0934 2020-02-27    65 recovered
    ## 2260          Anhui Mainland China 31.82570 117.2264 2020-02-28     1 confirmed
    ## 2261      Guangdong Mainland China 23.34170 113.4244 2020-02-28     1 confirmed
    ## 2262          Hebei Mainland China 38.04280 114.5149 2020-02-28     1 confirmed
    ## 2264          Hubei Mainland China 30.97560 112.2707 2020-02-28   318 confirmed
    ## 2265        Jiangxi Mainland China 27.61400 115.7221 2020-02-28     1 confirmed
    ## 2266        Sichuan Mainland China 30.61710 102.7103 2020-02-28     4 confirmed
    ## 2271        Beijing Mainland China 40.18240 116.4142 2020-02-28     2     death
    ## 2273          Hubei Mainland China 30.97560 112.2707 2020-02-28    41     death
    ## 2274       Xinjiang Mainland China 41.11290  85.2401 2020-02-28     1     death
    ## 2280          Anhui Mainland China 31.82570 117.2264 2020-02-28    29 recovered
    ## 2281        Beijing Mainland China 40.18240 116.4142 2020-02-28     9 recovered
    ## 2283      Chongqing Mainland China 30.05720 107.8740 2020-02-28    21 recovered
    ## 2284         Fujian Mainland China 26.07890 117.9874 2020-02-28     7 recovered
    ## 2285          Gansu Mainland China 36.06110 103.8343 2020-02-28     1 recovered
    ## 2286      Guangdong Mainland China 23.34170 113.4244 2020-02-28    45 recovered
    ## 2287        Guangxi Mainland China 23.82980 108.7881 2020-02-28     7 recovered
    ## 2288         Hainan Mainland China 19.19590 109.7453 2020-02-28     2 recovered
    ## 2289          Hebei Mainland China 38.04280 114.5149 2020-02-28     3 recovered
    ## 2290   Heilongjiang Mainland China 47.86200 127.7615 2020-02-28    13 recovered
    ## 2291          Henan Mainland China 33.88202 113.6140 2020-02-28    44 recovered
    ## 2293          Hubei Mainland China 30.97560 112.2707 2020-02-28  3020 recovered
    ## 2294          Hunan Mainland China 27.61040 111.7088 2020-02-28    26 recovered
    ## 2295 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-28     2 recovered
    ## 2296        Jiangsu Mainland China 32.97110 119.4550 2020-02-28    17 recovered
    ## 2297        Jiangxi Mainland China 27.61400 115.7221 2020-02-28    36 recovered
    ## 2298          Jilin Mainland China 43.66610 126.1923 2020-02-28     6 recovered
    ## 2299        Shaanxi Mainland China 35.19170 108.8701 2020-02-28     4 recovered
    ## 2300       Shandong Mainland China 36.34270 118.1498 2020-02-28    18 recovered
    ## 2301       Shanghai Mainland China 31.20200 121.4491 2020-02-28     3 recovered
    ## 2302         Shanxi Mainland China 37.57770 112.2922 2020-02-28     5 recovered
    ## 2303        Sichuan Mainland China 30.61710 102.7103 2020-02-28    17 recovered
    ## 2305       Xinjiang Mainland China 41.11290  85.2401 2020-02-28     9 recovered
    ## 2306         Yunnan Mainland China 24.97400 101.4870 2020-02-28     6 recovered
    ## 2307       Zhejiang Mainland China 29.18320 120.0934 2020-02-28    43 recovered
    ## 2340        Beijing Mainland China 40.18240 116.4142 2020-02-29     1 confirmed
    ## 2343      Guangdong Mainland China 23.34170 113.4244 2020-02-29     1 confirmed
    ## 2345          Hubei Mainland China 30.97560 112.2707 2020-02-29   423 confirmed
    ## 2346          Hunan Mainland China 27.61040 111.7088 2020-02-29     1 confirmed
    ## 2348        Ningxia Mainland China 37.26920 106.1655 2020-02-29     1 confirmed
    ## 2362        Beijing Mainland China 40.18240 116.4142 2020-02-29     1     death
    ## 2363          Henan Mainland China 33.88202 113.6140 2020-02-29     1     death
    ## 2364          Hubei Mainland China 30.97560 112.2707 2020-02-29    45     death
    ## 2372          Anhui Mainland China 31.82570 117.2264 2020-02-29    47 recovered
    ## 2373        Beijing Mainland China 40.18240 116.4142 2020-02-29    14 recovered
    ## 2374      Chongqing Mainland China 30.05720 107.8740 2020-02-29    16 recovered
    ## 2375         Fujian Mainland China 26.07890 117.9874 2020-02-29     8 recovered
    ## 2376      Guangdong Mainland China 23.34170 113.4244 2020-02-29    48 recovered
    ## 2377        Guangxi Mainland China 23.82980 108.7881 2020-02-29     8 recovered
    ## 2378         Hainan Mainland China 19.19590 109.7453 2020-02-29    15 recovered
    ## 2379          Hebei Mainland China 38.04280 114.5149 2020-02-29     5 recovered
    ## 2380   Heilongjiang Mainland China 47.86200 127.7615 2020-02-29    18 recovered
    ## 2381          Henan Mainland China 33.88202 113.6140 2020-02-29    58 recovered
    ## 2383          Hubei Mainland China 30.97560 112.2707 2020-02-29  2590 recovered
    ## 2384          Hunan Mainland China 27.61040 111.7088 2020-02-29    16 recovered
    ## 2385 Inner Mongolia Mainland China 44.09350 113.9448 2020-02-29     4 recovered
    ## 2386        Jiangsu Mainland China 32.97110 119.4550 2020-02-29     8 recovered
    ## 2387        Jiangxi Mainland China 27.61400 115.7221 2020-02-29    21 recovered
    ## 2388          Jilin Mainland China 43.66610 126.1923 2020-02-29     2 recovered
    ## 2389       Liaoning Mainland China 41.29560 122.6085 2020-02-29     3 recovered
    ## 2390        Ningxia Mainland China 37.26920 106.1655 2020-02-29     1 recovered
    ## 2391        Shaanxi Mainland China 35.19170 108.8701 2020-02-29     8 recovered
    ## 2392       Shandong Mainland China 36.34270 118.1498 2020-02-29    16 recovered
    ## 2393       Shanghai Mainland China 31.20200 121.4491 2020-02-29     8 recovered
    ## 2394         Shanxi Mainland China 37.57770 112.2922 2020-02-29     2 recovered
    ## 2395        Sichuan Mainland China 30.61710 102.7103 2020-02-29    13 recovered
    ## 2397        Tianjin Mainland China 39.30540 117.3230 2020-02-29     7 recovered
    ## 2398       Xinjiang Mainland China 41.11290  85.2401 2020-02-29    10 recovered
    ## 2399         Yunnan Mainland China 24.97400 101.4870 2020-02-29     1 recovered
    ## 2400       Zhejiang Mainland China 29.18320 120.0934 2020-02-29    41 recovered
    ## 2435        Beijing Mainland China 40.18240 116.4142 2020-03-01     2 confirmed
    ## 2438          Hubei Mainland China 30.97560 112.2707 2020-03-01   570 confirmed
    ## 2440       Liaoning Mainland China 41.29560 122.6085 2020-03-01     1 confirmed
    ## 2443       Shandong Mainland China 36.34270 118.1498 2020-03-01     2 confirmed
    ## 2452          Henan Mainland China 33.88202 113.6140 2020-03-01     1     death
    ## 2453          Hubei Mainland China 30.97560 112.2707 2020-03-01    34     death
    ## 2458          Anhui Mainland China 31.82570 117.2264 2020-03-01     5 recovered
    ## 2459        Beijing Mainland China 40.18240 116.4142 2020-03-01     5 recovered
    ## 2460      Chongqing Mainland China 30.05720 107.8740 2020-03-01    12 recovered
    ## 2461         Fujian Mainland China 26.07890 117.9874 2020-03-01     4 recovered
    ## 2462          Gansu Mainland China 36.06110 103.8343 2020-03-01     2 recovered
    ## 2463      Guangdong Mainland China 23.34170 113.4244 2020-03-01    33 recovered
    ## 2464        Guangxi Mainland China 23.82980 108.7881 2020-03-01     5 recovered
    ## 2465         Hainan Mainland China 19.19590 109.7453 2020-03-01     1 recovered
    ## 2466          Hebei Mainland China 38.04280 114.5149 2020-03-01    12 recovered
    ## 2467   Heilongjiang Mainland China 47.86200 127.7615 2020-03-01    41 recovered
    ## 2468          Henan Mainland China 33.88202 113.6140 2020-03-01    28 recovered
    ## 2470          Hubei Mainland China 30.97560 112.2707 2020-03-01  2543 recovered
    ## 2471          Hunan Mainland China 27.61040 111.7088 2020-03-01    20 recovered
    ## 2472 Inner Mongolia Mainland China 44.09350 113.9448 2020-03-01     3 recovered
    ## 2473        Jiangsu Mainland China 32.97110 119.4550 2020-03-01    13 recovered
    ## 2474        Jiangxi Mainland China 27.61400 115.7221 2020-03-01    20 recovered
    ## 2475          Jilin Mainland China 43.66610 126.1923 2020-03-01     3 recovered
    ## 2476       Liaoning Mainland China 41.29560 122.6085 2020-03-01     7 recovered
    ## 2477        Shaanxi Mainland China 35.19170 108.8701 2020-03-01     1 recovered
    ## 2478       Shandong Mainland China 36.34270 118.1498 2020-03-01    22 recovered
    ## 2479       Shanghai Mainland China 31.20200 121.4491 2020-03-01     3 recovered
    ## 2480         Shanxi Mainland China 37.57770 112.2922 2020-03-01     2 recovered
    ## 2481        Sichuan Mainland China 30.61710 102.7103 2020-03-01    14 recovered
    ## 2482        Tianjin Mainland China 39.30540 117.3230 2020-03-01     2 recovered
    ## 2483       Xinjiang Mainland China 41.11290  85.2401 2020-03-01     2 recovered
    ## 2484         Yunnan Mainland China 24.97400 101.4870 2020-03-01     6 recovered
    ## 2485       Zhejiang Mainland China 29.18320 120.0934 2020-03-01    30 recovered
    ## 2518        Beijing Mainland China 40.18240 116.4142 2020-03-02     1 confirmed
    ## 2521      Guangdong Mainland China 23.34170 113.4244 2020-03-02     1 confirmed
    ## 2524          Hubei Mainland China 30.97560 112.2707 2020-03-02   196 confirmed
    ## 2527        Ningxia Mainland China 37.26920 106.1655 2020-03-02     1 confirmed
    ## 2542       Zhejiang Mainland China 29.18320 120.0934 2020-03-02     1 confirmed
    ## 2547          Hubei Mainland China 30.97560 112.2707 2020-03-02    42     death
    ## 2554          Anhui Mainland China 31.82570 117.2264 2020-03-02    44 recovered
    ## 2555        Beijing Mainland China 40.18240 116.4142 2020-03-02     6 recovered
    ## 2556      Chongqing Mainland China 30.05720 107.8740 2020-03-02    19 recovered
    ## 2557         Fujian Mainland China 26.07890 117.9874 2020-03-02     8 recovered
    ## 2558          Gansu Mainland China 36.06110 103.8343 2020-03-02     1 recovered
    ## 2559      Guangdong Mainland China 23.34170 113.4244 2020-03-02    43 recovered
    ## 2560        Guangxi Mainland China 23.82980 108.7881 2020-03-02    11 recovered
    ## 2561        Guizhou Mainland China 26.81540 106.8748 2020-03-02     2 recovered
    ## 2562         Hainan Mainland China 19.19590 109.7453 2020-03-02     2 recovered
    ## 2563          Hebei Mainland China 38.04280 114.5149 2020-03-02     2 recovered
    ## 2564   Heilongjiang Mainland China 47.86200 127.7615 2020-03-02    14 recovered
    ## 2565          Henan Mainland China 33.88202 113.6140 2020-03-02     7 recovered
    ## 2566          Hubei Mainland China 30.97560 112.2707 2020-03-02  2398 recovered
    ## 2567          Hunan Mainland China 27.61040 111.7088 2020-03-02    21 recovered
    ## 2568 Inner Mongolia Mainland China 44.09350 113.9448 2020-03-02     2 recovered
    ## 2569        Jiangsu Mainland China 32.97110 119.4550 2020-03-02     7 recovered
    ## 2570        Jiangxi Mainland China 27.61400 115.7221 2020-03-02    19 recovered
    ## 2571          Jilin Mainland China 43.66610 126.1923 2020-03-02     5 recovered
    ## 2572        Shaanxi Mainland China 35.19170 108.8701 2020-03-02     8 recovered
    ## 2573       Shandong Mainland China 36.34270 118.1498 2020-03-02    17 recovered
    ## 2574       Shanghai Mainland China 31.20200 121.4491 2020-03-02     2 recovered
    ## 2575         Shanxi Mainland China 37.57770 112.2922 2020-03-02     3 recovered
    ## 2576        Sichuan Mainland China 30.61710 102.7103 2020-03-02    21 recovered
    ## 2578       Xinjiang Mainland China 41.11290  85.2401 2020-03-02     2 recovered
    ## 2579         Yunnan Mainland China 24.97400 101.4870 2020-03-02     5 recovered
    ## 2580       Zhejiang Mainland China 29.18320 120.0934 2020-03-02    23 recovered
    ## 2624          Hubei Mainland China 30.97560 112.2707 2020-03-03   114 confirmed
    ## 2626       Liaoning Mainland China 41.29560 122.6085 2020-03-03     3 confirmed
    ## 2632       Shanghai Mainland China 31.20200 121.4491 2020-03-03     1 confirmed
    ## 2638       Zhejiang Mainland China 29.18320 120.0934 2020-03-03     7 confirmed
    ## 2644          Hubei Mainland China 30.97560 112.2707 2020-03-03    32     death
    ## 2645 Inner Mongolia Mainland China 44.09350 113.9448 2020-03-03     1     death
    ## 2653          Anhui Mainland China 31.82570 117.2264 2020-03-03    19 recovered
    ## 2654        Beijing Mainland China 40.18240 116.4142 2020-03-03     6 recovered
    ## 2655      Chongqing Mainland China 30.05720 107.8740 2020-03-03    21 recovered
    ## 2656         Fujian Mainland China 26.07890 117.9874 2020-03-03     5 recovered
    ## 2657          Gansu Mainland China 36.06110 103.8343 2020-03-03     1 recovered
    ## 2658      Guangdong Mainland China 23.34170 113.4244 2020-03-03    42 recovered
    ## 2659        Guangxi Mainland China 23.82980 108.7881 2020-03-03    10 recovered
    ## 2660         Hainan Mainland China 19.19590 109.7453 2020-03-03     4 recovered
    ## 2661          Hebei Mainland China 38.04280 114.5149 2020-03-03     4 recovered
    ## 2662   Heilongjiang Mainland China 47.86200 127.7615 2020-03-03    10 recovered
    ## 2663          Henan Mainland China 33.88202 113.6140 2020-03-03    26 recovered
    ## 2665          Hubei Mainland China 30.97560 112.2707 2020-03-03  2274 recovered
    ## 2666          Hunan Mainland China 27.61040 111.7088 2020-03-03    19 recovered
    ## 2667 Inner Mongolia Mainland China 44.09350 113.9448 2020-03-03     5 recovered
    ## 2668        Jiangsu Mainland China 32.97110 119.4550 2020-03-03    19 recovered
    ## 2669        Jiangxi Mainland China 27.61400 115.7221 2020-03-03    20 recovered
    ## 2670       Liaoning Mainland China 41.29560 122.6085 2020-03-03     3 recovered
    ## 2673       Shandong Mainland China 36.34270 118.1498 2020-03-03    51 recovered
    ## 2674       Shanghai Mainland China 31.20200 121.4491 2020-03-03     2 recovered
    ## 2675         Shanxi Mainland China 37.57770 112.2922 2020-03-03     5 recovered
    ## 2676        Sichuan Mainland China 30.61710 102.7103 2020-03-03     8 recovered
    ## 2677        Tianjin Mainland China 39.30540 117.3230 2020-03-03    13 recovered
    ## 2678       Xinjiang Mainland China 41.11290  85.2401 2020-03-03     2 recovered
    ## 2679         Yunnan Mainland China 24.97400 101.4870 2020-03-03     1 recovered
    ## 2680       Zhejiang Mainland China 29.18320 120.0934 2020-03-03    24 recovered
    ## 2724        Beijing Mainland China 40.18240 116.4142 2020-03-04     4 confirmed
    ## 2728          Hubei Mainland China 30.97560 112.2707 2020-03-04   115 confirmed
    ## 2732        Ningxia Mainland China 37.26920 106.1655 2020-03-04     1 confirmed
    ## 2745          Hubei Mainland China 30.97560 112.2707 2020-03-04    36     death
    ## 2755          Anhui Mainland China 31.82570 117.2264 2020-03-04    20 recovered
    ## 2756        Beijing Mainland China 40.18240 116.4142 2020-03-04     9 recovered
    ## 2757      Chongqing Mainland China 30.05720 107.8740 2020-03-04    12 recovered
    ## 2758         Fujian Mainland China 26.07890 117.9874 2020-03-04    10 recovered
    ## 2759          Gansu Mainland China 36.06110 103.8343 2020-03-04     1 recovered
    ## 2760      Guangdong Mainland China 23.34170 113.4244 2020-03-04    32 recovered
    ## 2761        Guangxi Mainland China 23.82980 108.7881 2020-03-04     8 recovered
    ## 2762         Hainan Mainland China 19.19590 109.7453 2020-03-04     3 recovered
    ## 2763          Hebei Mainland China 38.04280 114.5149 2020-03-04     1 recovered
    ## 2764   Heilongjiang Mainland China 47.86200 127.7615 2020-03-04     7 recovered
    ## 2765          Henan Mainland China 33.88202 113.6140 2020-03-04     3 recovered
    ## 2766          Hubei Mainland China 30.97560 112.2707 2020-03-04  2349 recovered
    ## 2767          Hunan Mainland China 27.61040 111.7088 2020-03-04    10 recovered
    ## 2768 Inner Mongolia Mainland China 44.09350 113.9448 2020-03-04     4 recovered
    ## 2769        Jiangsu Mainland China 32.97110 119.4550 2020-03-04    15 recovered
    ## 2770        Jiangxi Mainland China 27.61400 115.7221 2020-03-04    14 recovered
    ## 2771          Jilin Mainland China 43.66610 126.1923 2020-03-04     3 recovered
    ## 2772        Shaanxi Mainland China 35.19170 108.8701 2020-03-04     7 recovered
    ## 2773       Shandong Mainland China 36.34270 118.1498 2020-03-04     5 recovered
    ## 2774       Shanghai Mainland China 31.20200 121.4491 2020-03-04     4 recovered
    ## 2775        Sichuan Mainland China 30.61710 102.7103 2020-03-04    12 recovered
    ## 2776       Xinjiang Mainland China 41.11290  85.2401 2020-03-04     1 recovered
    ## 2777       Zhejiang Mainland China 29.18320 120.0934 2020-03-04    21 recovered
    ## 2827          Gansu Mainland China 36.06110 103.8343 2020-03-05    11 confirmed
    ## 2829      Guangdong Mainland China 23.34170 113.4244 2020-03-05     1 confirmed
    ## 2831   Heilongjiang Mainland China 47.86200 127.7615 2020-03-05     1 confirmed
    ## 2832          Hubei Mainland China 30.97560 112.2707 2020-03-05   134 confirmed
    ## 2842       Shanghai Mainland China 31.20200 121.4491 2020-03-05     1 confirmed
    ## 2843        Sichuan Mainland China 30.61710 102.7103 2020-03-05     1 confirmed
    ## 2850       Zhejiang Mainland China 29.18320 120.0934 2020-03-05     2 confirmed
    ## 2857         Hainan Mainland China 19.19590 109.7453 2020-03-05     1     death
    ## 2858          Hubei Mainland China 30.97560 112.2707 2020-03-05    31     death
    ## 2862          Anhui Mainland China 31.82570 117.2264 2020-03-05    14 recovered
    ## 2863      Chongqing Mainland China 30.05720 107.8740 2020-03-05    10 recovered
    ## 2864         Fujian Mainland China 26.07890 117.9874 2020-03-05     7 recovered
    ## 2865      Guangdong Mainland China 23.34170 113.4244 2020-03-05    48 recovered
    ## 2866        Guangxi Mainland China 23.82980 108.7881 2020-03-05     4 recovered
    ## 2867          Hebei Mainland China 38.04280 114.5149 2020-03-05     3 recovered
    ## 2868   Heilongjiang Mainland China 47.86200 127.7615 2020-03-05     6 recovered
    ## 2869          Henan Mainland China 33.88202 113.6140 2020-03-05     5 recovered
    ## 2871          Hubei Mainland China 30.97560 112.2707 2020-03-05  2035 recovered
    ## 2872          Hunan Mainland China 27.61040 111.7088 2020-03-05    22 recovered
    ## 2873 Inner Mongolia Mainland China 44.09350 113.9448 2020-03-05     2 recovered
    ## 2874        Jiangsu Mainland China 32.97110 119.4550 2020-03-05     6 recovered
    ## 2875        Jiangxi Mainland China 27.61400 115.7221 2020-03-05    17 recovered
    ## 2876          Jilin Mainland China 43.66610 126.1923 2020-03-05     2 recovered
    ## 2878        Shaanxi Mainland China 35.19170 108.8701 2020-03-05     1 recovered
    ## 2879       Shandong Mainland China 36.34270 118.1498 2020-03-05    62 recovered
    ## 2880       Shanghai Mainland China 31.20200 121.4491 2020-03-05     5 recovered
    ## 2881         Shanxi Mainland China 37.57770 112.2922 2020-03-05     2 recovered
    ## 2882        Sichuan Mainland China 30.61710 102.7103 2020-03-05    19 recovered
    ## 2883        Tianjin Mainland China 39.30540 117.3230 2020-03-05     4 recovered
    ## 2885       Xinjiang Mainland China 41.11290  85.2401 2020-03-05     1 recovered
    ## 2886       Zhejiang Mainland China 29.18320 120.0934 2020-03-05    10 recovered

> Q4. What is the death rate in “Mainland China”?

``` r
mainland_total_cases <- sum(mainland$cases)
mainland_total_cases
```

    ## [1] 135675

``` r
mainland_inds <-  mainland$type == 'death'
mainland_death_cases <- sum(mainland[mainland_inds,"cases"])
mainland_death_cases
```

    ## [1] 3013

``` r
round(mainland_death_cases/mainland_total_cases,4)
```

    ## [1] 0.0222

> Q5. What is the death rate in Italy, Iran and the US?

``` r
italy <-  virus[virus$Country.Region == "Italy",]
italy 
```

    ##      Province.State Country.Region Lat Long       date cases      type
    ## 369                          Italy  43   12 2020-01-31     2 confirmed
    ## 788                          Italy  43   12 2020-02-07     1 confirmed
    ## 1778                         Italy  43   12 2020-02-21    17 confirmed
    ## 1806                         Italy  43   12 2020-02-21     1     death
    ## 1849                         Italy  43   12 2020-02-22    42 confirmed
    ## 1870                         Italy  43   12 2020-02-22     1     death
    ## 1875                         Italy  43   12 2020-02-22     1 recovered
    ## 1905                         Italy  43   12 2020-02-23    93 confirmed
    ## 1920                         Italy  43   12 2020-02-23     1     death
    ## 1925                         Italy  43   12 2020-02-23     1 recovered
    ## 1964                         Italy  43   12 2020-02-24    74 confirmed
    ## 1986                         Italy  43   12 2020-02-24     4     death
    ## 1990                         Italy  43   12 2020-02-24    -1 recovered
    ## 2029                         Italy  43   12 2020-02-25    93 confirmed
    ## 2049                         Italy  43   12 2020-02-25     3     death
    ## 2099                         Italy  43   12 2020-02-26   131 confirmed
    ## 2123                         Italy  43   12 2020-02-26     2     death
    ## 2130                         Italy  43   12 2020-02-26     2 recovered
    ## 2171                         Italy  43   12 2020-02-27   202 confirmed
    ## 2196                         Italy  43   12 2020-02-27     5     death
    ## 2205                         Italy  43   12 2020-02-27    42 recovered
    ## 2245                         Italy  43   12 2020-02-28   233 confirmed
    ## 2270                         Italy  43   12 2020-02-28     4     death
    ## 2277                         Italy  43   12 2020-02-28     1 recovered
    ## 2320                         Italy  43   12 2020-02-29   240 confirmed
    ## 2359                         Italy  43   12 2020-02-29     8     death
    ## 2421                         Italy  43   12 2020-03-01   566 confirmed
    ## 2448                         Italy  43   12 2020-03-01     5     death
    ## 2456                         Italy  43   12 2020-03-01    37 recovered
    ## 2498                         Italy  43   12 2020-03-02   342 confirmed
    ## 2545                         Italy  43   12 2020-03-02    18     death
    ## 2551                         Italy  43   12 2020-03-02    66 recovered
    ## 2598                         Italy  43   12 2020-03-03   466 confirmed
    ## 2641                         Italy  43   12 2020-03-03    27     death
    ## 2647                         Italy  43   12 2020-03-03    11 recovered
    ## 2703                         Italy  43   12 2020-03-04   587 confirmed
    ## 2742                         Italy  43   12 2020-03-04    28     death
    ## 2750                         Italy  43   12 2020-03-04   116 recovered
    ## 2797                         Italy  43   12 2020-03-05   769 confirmed
    ## 2853                         Italy  43   12 2020-03-05    41     death
    ## 2861                         Italy  43   12 2020-03-05   138 recovered

``` r
Iran <-  virus[virus$Country.Region == "Iran",]
Iran 
```

    ##      Province.State Country.Region Lat Long       date cases      type
    ## 1641                          Iran  32   53 2020-02-19     2 confirmed
    ## 1668                          Iran  32   53 2020-02-19     2     death
    ## 1709                          Iran  32   53 2020-02-20     3 confirmed
    ## 1776                          Iran  32   53 2020-02-21    13 confirmed
    ## 1805                          Iran  32   53 2020-02-21     2     death
    ## 1848                          Iran  32   53 2020-02-22    10 confirmed
    ## 1869                          Iran  32   53 2020-02-22     1     death
    ## 1904                          Iran  32   53 2020-02-23    15 confirmed
    ## 1919                          Iran  32   53 2020-02-23     3     death
    ## 1962                          Iran  32   53 2020-02-24    18 confirmed
    ## 1985                          Iran  32   53 2020-02-24     4     death
    ## 2028                          Iran  32   53 2020-02-25    34 confirmed
    ## 2048                          Iran  32   53 2020-02-25     4     death
    ## 2096                          Iran  32   53 2020-02-26    44 confirmed
    ## 2122                          Iran  32   53 2020-02-26     3     death
    ## 2129                          Iran  32   53 2020-02-26    49 recovered
    ## 2168                          Iran  32   53 2020-02-27   106 confirmed
    ## 2195                          Iran  32   53 2020-02-27     7     death
    ## 2243                          Iran  32   53 2020-02-28   143 confirmed
    ## 2269                          Iran  32   53 2020-02-28     8     death
    ## 2276                          Iran  32   53 2020-02-28    24 recovered
    ## 2316                          Iran  32   53 2020-02-29   205 confirmed
    ## 2358                          Iran  32   53 2020-02-29     9     death
    ## 2367                          Iran  32   53 2020-02-29    50 recovered
    ## 2418                          Iran  32   53 2020-03-01   385 confirmed
    ## 2447                          Iran  32   53 2020-03-01    11     death
    ## 2455                          Iran  32   53 2020-03-01    52 recovered
    ## 2496                          Iran  32   53 2020-03-02   523 confirmed
    ## 2544                          Iran  32   53 2020-03-02    12     death
    ## 2550                          Iran  32   53 2020-03-02   116 recovered
    ## 2594                          Iran  32   53 2020-03-03   835 confirmed
    ## 2640                          Iran  32   53 2020-03-03    11     death
    ## 2699                          Iran  32   53 2020-03-04   586 confirmed
    ## 2740                          Iran  32   53 2020-03-04    15     death
    ## 2749                          Iran  32   53 2020-03-04   261 recovered
    ## 2795                          Iran  32   53 2020-03-05   591 confirmed
    ## 2852                          Iran  32   53 2020-03-05    15     death
    ## 2860                          Iran  32   53 2020-03-05   187 recovered

``` r
US <-  virus[virus$Country.Region == "US",]
US
```

    ##                                   Province.State Country.Region     Lat
    ## 18                               King County, WA             US 47.6062
    ## 70                               Cook County, IL             US 41.7377
    ## 160                              Los Angeles, CA             US 34.0522
    ## 164                            Orange County, CA             US 33.7879
    ## 171                                    Tempe, AZ             US 33.4255
    ## 380                              Cook County, IL             US 41.7377
    ## 400                              Santa Clara, CA             US 37.3541
    ## 436                                   Boston, MA             US 42.3601
    ## 569                               San Benito, CA             US 36.5761
    ## 570                              Santa Clara, CA             US 37.3541
    ## 685                                  Madison, WI             US 43.0731
    ## 978                              Cook County, IL             US 41.7377
    ## 992                              King County, WA             US 47.6062
    ## 1091                        San Diego County, CA             US 32.7157
    ## 1244                             San Antonio, TX             US 29.4241
    ## 1245                        San Diego County, CA             US 32.7157
    ## 1794                         Humboldt County, CA             US 40.7450
    ## 1796        Lackland, TX (From Diamond Princess)             US 29.3829
    ## 1797           Omaha, NE (From Diamond Princess)             US 41.2545
    ## 1798                       Sacramento County, CA             US 38.4747
    ## 1803          Travis, CA (From Diamond Princess)             US 38.2721
    ## 1836                        San Diego County, CA             US 32.7157
    ## 1837                             Santa Clara, CA             US 37.3541
    ## 1976        Lackland, TX (From Diamond Princess)             US 29.3829
    ## 1977           Omaha, NE (From Diamond Princess)             US 41.2545
    ## 1983          Travis, CA (From Diamond Princess)             US 38.2721
    ## 1984 Unassigned Location (From Diamond Princess)             US 35.4437
    ## 2084                                   Tempe, AZ             US 33.4255
    ## 2120 Unassigned Location (From Diamond Princess)             US 35.4437
    ## 2191                       Sacramento County, CA             US 38.4747
    ## 2268 Unassigned Location (From Diamond Princess)             US 35.4437
    ## 2282                                  Boston, MA             US 42.3601
    ## 2347                             King County, WA             US 47.6062
    ## 2350                             Santa Clara, CA             US 37.3541
    ## 2351                        Snohomish County, WA             US 48.0330
    ## 2356                       Washington County, OR             US 45.5470
    ## 2365                             King County, WA             US 47.6062
    ## 2436                             Cook County, IL             US 41.7377
    ## 2439                             King County, WA             US 47.6062
    ## 2442                              Providence, RI             US 41.8240
    ## 2444                        Snohomish County, WA             US 48.0330
    ## 2519                             Cook County, IL             US 41.7377
    ## 2520                          Grafton County, NH             US 43.9088
    ## 2522                            Hillsborough, FL             US 27.9904
    ## 2525                             King County, WA             US 47.6062
    ## 2526                           New York City, NY             US 40.7128
    ## 2528                           Placer County, CA             US 39.0916
    ## 2529                              Providence, RI             US 41.8240
    ## 2530                               San Mateo, CA             US 37.5630
    ## 2531                             Santa Clara, CA             US 37.3541
    ## 2532                                Sarasota, FL             US 27.3364
    ## 2533                        Snohomish County, WA             US 48.0330
    ## 2534                           Sonoma County, CA             US 38.5780
    ## 2538                                Umatilla, OR             US 45.7750
    ## 2539 Unassigned Location (From Diamond Princess)             US 35.4437
    ## 2541                       Washington County, OR             US 45.5470
    ## 2548                             King County, WA             US 47.6062
    ## 2549                        Snohomish County, WA             US 48.0330
    ## 2617                          Norfolk County, MA             US 42.1767
    ## 2618                                Berkeley, CA             US 37.8715
    ## 2621                           Fulton County, GA             US 33.8034
    ## 2622                          Grafton County, NH             US 43.9088
    ## 2623                            Hillsborough, FL             US 27.9904
    ## 2625                             King County, WA             US 47.6062
    ## 2627                         Maricopa County, AZ             US 33.2918
    ## 2630                               San Mateo, CA             US 37.5630
    ## 2631                             Santa Clara, CA             US 37.3541
    ## 2633                        Snohomish County, WA             US 48.0330
    ## 2636                             Wake County, NC             US 35.8032
    ## 2637                      Westchester County, NY             US 41.1220
    ## 2646                             King County, WA             US 47.6062
    ## 2672                                 Madison, WI             US 43.0731
    ## 2726                     Contra Costa County, CA             US 37.8534
    ## 2729                             King County, WA             US 47.6062
    ## 2730                             Los Angeles, CA             US 34.0522
    ## 2734                           Orange County, CA             US 33.7879
    ## 2735                           Placer County, CA             US 39.0916
    ## 2736                        Snohomish County, WA             US 48.0330
    ## 2739                      Westchester County, NY             US 41.1220
    ## 2746                             King County, WA             US 47.6062
    ## 2748                           Placer County, CA             US 39.0916
    ## 2822                           Bergen County, NJ             US 40.9263
    ## 2824                            Clark County, NV             US 36.0796
    ## 2825                             Cook County, IL             US 41.7377
    ## 2826                        Fort Bend County, TX             US 29.5693
    ## 2828                            Grant County, WA             US 47.1981
    ## 2830                           Harris County, TX             US 29.7752
    ## 2833                             King County, WA             US 47.6062
    ## 2834                             Los Angeles, CA             US 34.0522
    ## 2835                           New York City, NY             US 40.7128
    ## 2836                           Queens County, NY             US 40.7282
    ## 2838                        San Diego County, CA             US 32.7157
    ## 2839                    San Francisco County, CA             US 37.7749
    ## 2840                             Santa Clara, CA             US 37.3541
    ## 2841                       Santa Rosa County, FL             US 30.7690
    ## 2844                        Snohomish County, WA             US 48.0330
    ## 2847                      Westchester County, NY             US 41.1220
    ## 2849                       Williamson County, TN             US 35.9179
    ## 2859                             King County, WA             US 47.6062
    ##           Long       date cases      type
    ## 18   -122.3321 2020-01-22     1 confirmed
    ## 70    -87.6976 2020-01-24     1 confirmed
    ## 160  -118.2437 2020-01-26     1 confirmed
    ## 164  -117.8531 2020-01-26     1 confirmed
    ## 171  -111.9400 2020-01-26     1 confirmed
    ## 380   -87.6976 2020-01-31     1 confirmed
    ## 400  -121.9552 2020-01-31     1 confirmed
    ## 436   -71.0589 2020-02-01     1 confirmed
    ## 569  -120.9876 2020-02-03     2 confirmed
    ## 570  -121.9552 2020-02-03     1 confirmed
    ## 685   -89.4012 2020-02-05     1 confirmed
    ## 978   -87.6976 2020-02-09     2 recovered
    ## 992  -122.3321 2020-02-09     1 recovered
    ## 1091 -117.1611 2020-02-11     1 confirmed
    ## 1244  -98.4936 2020-02-13     1 confirmed
    ## 1245 -117.1611 2020-02-13     1 confirmed
    ## 1794 -123.8695 2020-02-21     1 confirmed
    ## 1796  -98.6134 2020-02-21     2 confirmed
    ## 1797  -95.9758 2020-02-21    11 confirmed
    ## 1798 -121.3542 2020-02-21     1 confirmed
    ## 1803 -121.9399 2020-02-21     5 confirmed
    ## 1836 -117.1611 2020-02-21     1 recovered
    ## 1837 -121.9552 2020-02-21     1 recovered
    ## 1976  -98.6134 2020-02-24    -2 confirmed
    ## 1977  -95.9758 2020-02-24   -11 confirmed
    ## 1983 -121.9399 2020-02-24    -5 confirmed
    ## 1984  139.6380 2020-02-24    36 confirmed
    ## 2084 -111.9400 2020-02-25     1 recovered
    ## 2120  139.6380 2020-02-26     6 confirmed
    ## 2191 -121.3542 2020-02-27     1 confirmed
    ## 2268  139.6380 2020-02-28     2 confirmed
    ## 2282  -71.0589 2020-02-28     1 recovered
    ## 2347 -122.3321 2020-02-29     5 confirmed
    ## 2350 -121.9552 2020-02-29     1 confirmed
    ## 2351 -121.8339 2020-02-29     1 confirmed
    ## 2356 -123.1386 2020-02-29     1 confirmed
    ## 2365 -122.3321 2020-02-29     1     death
    ## 2436  -87.6976 2020-03-01     1 confirmed
    ## 2439 -122.3321 2020-03-01     3 confirmed
    ## 2442  -71.4128 2020-03-01     1 confirmed
    ## 2444 -121.8339 2020-03-01     1 confirmed
    ## 2519  -87.6976 2020-03-02     1 confirmed
    ## 2520  -71.8260 2020-03-02     1 confirmed
    ## 2522  -82.3018 2020-03-02     1 confirmed
    ## 2525 -122.3321 2020-03-02     5 confirmed
    ## 2526  -74.0060 2020-03-02     1 confirmed
    ## 2528 -120.8039 2020-03-02     1 confirmed
    ## 2529  -71.4128 2020-03-02     1 confirmed
    ## 2530 -122.3255 2020-03-02     1 confirmed
    ## 2531 -121.9552 2020-03-02     6 confirmed
    ## 2532  -82.5307 2020-03-02     1 confirmed
    ## 2533 -121.8339 2020-03-02     2 confirmed
    ## 2534 -122.9888 2020-03-02     1 confirmed
    ## 2538 -118.7606 2020-03-02     1 confirmed
    ## 2539  139.6380 2020-03-02     1 confirmed
    ## 2541 -123.1386 2020-03-02     1 confirmed
    ## 2548 -122.3321 2020-03-02     4     death
    ## 2549 -121.8339 2020-03-02     1     death
    ## 2617  -71.1449 2020-03-03     1 confirmed
    ## 2618 -122.2730 2020-03-03     1 confirmed
    ## 2621  -84.3963 2020-03-03     2 confirmed
    ## 2622  -71.8260 2020-03-03     1 confirmed
    ## 2623  -82.3018 2020-03-03     1 confirmed
    ## 2625 -122.3321 2020-03-03     7 confirmed
    ## 2627 -112.4291 2020-03-03     1 confirmed
    ## 2630 -122.3255 2020-03-03     1 confirmed
    ## 2631 -121.9552 2020-03-03     2 confirmed
    ## 2633 -121.8339 2020-03-03     2 confirmed
    ## 2636  -78.5661 2020-03-03     1 confirmed
    ## 2637  -73.7949 2020-03-03     1 confirmed
    ## 2646 -122.3321 2020-03-03     1     death
    ## 2672  -89.4012 2020-03-03     1 recovered
    ## 2726 -121.9018 2020-03-04     1 confirmed
    ## 2729 -122.3321 2020-03-04    10 confirmed
    ## 2730 -118.2437 2020-03-04     6 confirmed
    ## 2734 -117.8531 2020-03-04     2 confirmed
    ## 2735 -120.8039 2020-03-04     1 confirmed
    ## 2736 -121.8339 2020-03-04     2 confirmed
    ## 2739  -73.7949 2020-03-04     9 confirmed
    ## 2746 -122.3321 2020-03-04     3     death
    ## 2748 -120.8039 2020-03-04     1     death
    ## 2822  -74.0770 2020-03-05     2 confirmed
    ## 2824 -115.0940 2020-03-05     1 confirmed
    ## 2825  -87.6976 2020-03-05     1 confirmed
    ## 2826  -95.8143 2020-03-05     1 confirmed
    ## 2828 -119.3732 2020-03-05     1 confirmed
    ## 2830  -95.3103 2020-03-05     2 confirmed
    ## 2833 -122.3321 2020-03-05    20 confirmed
    ## 2834 -118.2437 2020-03-05     4 confirmed
    ## 2835  -74.0060 2020-03-05     3 confirmed
    ## 2836  -73.7949 2020-03-05     1 confirmed
    ## 2838 -117.1611 2020-03-05     1 confirmed
    ## 2839 -122.4194 2020-03-05     2 confirmed
    ## 2840 -121.9552 2020-03-05     9 confirmed
    ## 2841  -86.9824 2020-03-05     1 confirmed
    ## 2844 -121.8339 2020-03-05    10 confirmed
    ## 2847  -73.7949 2020-03-05     8 confirmed
    ## 2849  -86.8622 2020-03-05     1 confirmed
    ## 2859 -122.3321 2020-03-05     1     death

``` r
combined_total_cases <- sum(italy$cases,US$cases,Iran$cases)
combined_total_cases
```

    ## [1] 9020

``` r
italy_inds <-  italy$type == 'death'
US_inds <-  US$type == 'death'
Iran_inds <-  Iran$type == 'death'
combined_death_cases <- sum(italy[italy_inds,"cases"],US[US_inds,"cases"],Iran[Iran_inds,"cases"])
combined_death_cases
```

    ## [1] 267

``` r
round(combined_death_cases/combined_total_cases,4)
```

    ## [1] 0.0296
